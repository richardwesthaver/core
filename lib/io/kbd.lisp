;;; kbd.lisp --- Keyboard-based IO

;; Keyboard-like devices and input

;;; Commentary:

;; refs:

;; - https://www.kernel.org/doc/Documentation/input/event-codes.txt

;; - https://github.com/xkbcommon/libxkbcommon/blob/master/tools/interactive-evdev.c

;; - https://gitlab.freedesktop.org/libevdev/libevdev/-/tree/master/tools

;;; Code:
(in-package :io/kbd)

;; load xkbcommon - NAME-KEYSYM is used at compile-time
(eval-when (:compile-toplevel :load-toplevel) (load-xkbcommon t))

(defun load-kbd-libs ()
  (load-xkbcommon)
  (load-evdev))

;;; Vars
(defconstant +evdev-offset+ 8)
(defconstant +long-bit+ (sb-alien:alien-size sb-alien:unsigned-long))
(defparameter *keyboards* nil)
(defvar *keysym-sets* nil
  "Alist of (NAME FIRST LAST).")
(defvar *keysym-character-table* (make-hash-table :test 'eql)
  "Table mapping Characters to KEYSYMs.")
(eval-always
  (defvar *keysym-name-table* (make-hash-table :test #'equal))
  (defvar *name-keysym-table* (make-hash-table :test #'equal)))
(defvar *dead-keysym-name-table* (make-hash-table))
(defvar *keymaps* (make-hash-table))
(defvar *keymap* nil)
(defvar *default-keysym-translate-mask*
  (logand #xff (lognot 2))
  ;; (make-state-mask :lock)
  "Default keysym state mask to use during keysym-translation.")
(defhook *keymap-hook* (:define))

(defconstant +unbound-keysym+ 0)

;;; Conditions
(define-condition kbd-error (error) ())
(deferror simple-kbd-error (simple-error kbd-error) () (:auto t))
(define-condition kbd-parse-error (kbd-error invalid-item) ()
  (:default-initargs :reason nil)
  (:report 
   (lambda (c s)
     (format s "Failed to parse key string: ~s" (error-item c))
     (when-let ((reason (error-reason c)))
       (format s "~%Reason: ~A" reason)))))
(definline kbd-parse-error (str &optional reason) (error 'kbd-parse-error :item str :reason reason))

;;; Keysym
(deftype keysym () '(unsigned-byte 32))

(definline keysym-name (code)
  (with-alien ((str (* unsigned-char) (make-alien unsigned-char 11)))
    (xkb::xkb-keysym-get-name code str 11)
    (cast str c-string)))

(definline name-keysym (name &optional (case-insensitive t))
  (let ((k (xkb-keysym-from-name name (if case-insensitive 1 0))))
    (unless (zerop k) k)))

(definline keysym-code-name (name)
  (gethash name *keysym-name-table*))

(defun (setf keysym-code-name) (new name)
  (setf (gethash name *keysym-name-table*) new))

(definline keysym-name-code (name)
  (gethash name *name-keysym-table*))

(defun (setf keysym-name-code) (new name)
  (setf (gethash name *name-keysym-table*) new))

(defmacro define-keysym-names (&body body)
  "Map the elements of each form to (SETF (KEYSYM-CODE-NAME 2) 1)."
  `(setf ,@(mapcan (lambda (x) (destructuring-bind (a b) x
                                 `((keysym-code-name ,a) ,b)))
                   body)))

(eval-always
  (defun name-from-keysym-name (name)
    (gethash name *keysym-name-table*))

  (defun name-from-keysym (key)
    (let ((k (keysym-code-name key)))
      (or (name-from-keysym-name k)
          k)))
  (defun keysym-from-name (name)
    "Return the keysym corresponding to NAME."
    (let ((f (name-from-keysym-name name)))
      (or (keysym-name-code (or f name))
          (name-keysym (or f name))))))

(defun load-xkb-keysyms (&rest codes)
  "Retrieve and map the names of the keysyms CODES which are all integers. Returns a table of INT->STRING."
  (dolist (c codes (values *keysym-name-table* *name-keysym-table*))
    (declare (fixnum c))
    (lety ((n (keysym-name c) :type string))
      (if (and (> (length n) 5) (string= "dead_" (subseq n 0 5)))
          (setf (gethash c *dead-keysym-name-table*) (subseq n 5))
          (setf (gethash c *keysym-name-table*) n
                (gethash n *name-keysym-table*) c)))))

(defun load-xkb-keysyms-file (file)
  (apply 'load-xkb-keysyms (read-lisp-file file)))

(eval-always
  (defun keysym (key &rest bytes)
    "Build a 32-bit keysym. KEY is an integer or character and BYTES optionally
fill in the lower bytes."
    (declare (dynamic-extent bytes))
    (etypecase key
      (keysym
       (dolist (b bytes key) (setq key (+ (ash key 8) b))))
      (character
       (or (gethash key *keysym-character-table*)
           (error "~s isn't a keysym" key))))))

(defun keysym-set (name)
  (cdr (assoc name *keysym-sets*)))

(defun (setf keysym-set) (cons name)
  (destructuring-bind (first last) cons
    (declare (keyword name)
             (keysym first last))
    (when (> first last)
      (rotatef first last))
    (setq *keysym-sets* (delete name *keysym-sets* :key #'car))
    (push (cons name cons) *keysym-sets*)))

(defun keysym-set-name (code)
  "Return the character code set name of keysym."
  (declare (keysym code))
  (dolist (set *keysym-sets*)
    (let ((first (second set))
          (last (third set)))
      (when (<= first code last)
        (return (first set))))))

(defmacro define-keysym-sets (&body body)
  "Define a set of keysym-sets via (SETF (KEYSYM-SET NAME) (LIST FIRST LAST) ...)"
  `(setf ,@(mapcan (lambda (x) (destructuring-bind (name first last) x
                                 `((keysym-set ,name) (list ,first ,last))))
                   body)))

(define-keysym-sets 
  (:latin-1 (keysym 0 0) (keysym 0 255))
  (:latin-2 (keysym 1 0) (keysym 1 255))
  (:latin-3 (keysym 2 0) (keysym 2 255))
  (:latin-4 (keysym 3 0) (keysym 3 255))
  (:kana (keysym 4 0) (keysym 4 255))
  (:arabic (keysym 5 0) (keysym 5 255))
  (:cyrillic (keysym 6 0) (keysym 6 255))
  (:greek (keysym 7 0) (keysym 7 255))
  (:tech (keysym 8 0) (keysym 8 255))
  (:special (keysym 9 0) (keysym 9 255))
  (:publish (keysym 10 0) (keysym 10 255))
  (:apl 	(keysym 11 0) (keysym 11 255))
  (:hebrew (keysym 12 0) (keysym 12 255))
  (:thai        (keysym 13 0) (keysym 13 255))
  (:korean      (keysym 14 0) (keysym 14 255))
  (:latin-5     (keysym 15 0) (keysym 15 255))
  (:latin-6     (keysym 16 0) (keysym 16 255))
  (:latin-7     (keysym 17 0) (keysym 17 255))
  (:latin-8     (keysym 18 0) (keysym 18 255))
  (:latin-9     (keysym 19 0) (keysym 19 255))
  (:currency    (keysym 32 0) (keysym 32 255))
  (:3270      (keysym 253 0) (keysym 253 255))
  (:xkb         (keysym 254 0) (keysym 254 255))
  (:keyboard (keysym 255 0) (keysym 255 255)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant character-set-switch-keysym (keysym 255 126))
  (defconstant left-shift-keysym (keysym 255 225))
  (defconstant right-shift-keysym (keysym 255 226))
  (defconstant left-control-keysym (keysym 255 227))
  (defconstant right-control-keysym (keysym 255 228))
  (defconstant caps-lock-keysym (keysym 255 229))
  (defconstant shift-lock-keysym (keysym 255 230))
  (defconstant left-meta-keysym (keysym 255 231))
  (defconstant right-meta-keysym (keysym 255 232))
  (defconstant left-alt-keysym (keysym 255 233))
  (defconstant right-alt-keysym (keysym 255 234))
  (defconstant left-super-keysym (keysym 255 235))
  (defconstant right-super-keysym (keysym 255 236))
  (defconstant left-hyper-keysym (keysym 255 237))
  (defconstant right-hyper-keysym (keysym 255 238)))

(defstruct (char-map (:type list)) char lower mods mask)

(defun define-keysym (obj keysym &key lower mods mask)
  "Define the translation from keysym/modifiers to a (usually
character) object. Any previous keysym definition with KEYSYM and MODS is
deleted before the new definition is added.

MODS is either a modifier-mask or list containing intermixed keysyms and
state-mask-keys specifying when to use this keysym-translation. The default is
NIL.

MASK is either a KEYMOD or list containing intermixed keysyms and
state-mask-keys specifying which modifiers to look at
(i.e. modifiers not specified are ignored).

If mask is :MODS then the mask is the same as the modifiers
(i.e. modifiers not specified by modifiers are don't cares) The default mask
is *default-keysym-translate-mask*

LOWER is used for uppercase alphabetic keysyms. The value is the associated
lowercase keysym. This information is used by the predicate (for caps-lock
computations) and by the keysym-downcase function."
  (declare ((or character keyword) obj)
           (keysym keysym)
           ((or (unsigned-byte 16) list) mods)
           ((or (member :modifiers) (unsigned-byte 16) list) mask)
           ((or null keysym) lower))
  (setf (gethash keysym *keysym-character-table*)
        (cond
          (mask
           (when (or (null mods) (and (numberp mods) (zerop mods)))
             (error "Mask with no modifiers"))
           (list obj lower mods mask))
          (mods (list obj lower mods))
          (lower (list obj lower))
          (t (list obj)))))

(defun undefine-keysym (obj keysym &optional mods)
  (declare ((or character t) obj)
           (keysym keysym)
           ((or (unsigned-byte 16) list) mods))
  (flet ((match (key entry)
           (let ((object (car key))
                 (modifiers (cdr key)))
             (or (eql object (char-map-char entry))
                 (equal modifiers (char-map-mods entry))))))
    (let ((previous (gethash keysym *keysym-character-table*))
          (key (cons obj mods)))
      (when (and previous (find key previous :test #'match))
        (setq previous (delete key previous :test #'match))
        (setf (gethash keysym *keysym-character-table*) previous)))))

(define-keysym :character-set-switch character-set-switch-keysym)
(define-keysym :left-shift left-shift-keysym)
(define-keysym :right-shift right-shift-keysym)
(define-keysym :left-control left-control-keysym)
(define-keysym :right-control right-control-keysym)
(define-keysym :caps-lock caps-lock-keysym)
(define-keysym :shift-lock shift-lock-keysym)
(define-keysym :left-meta left-meta-keysym)
(define-keysym :right-meta right-meta-keysym)
(define-keysym :left-alt left-alt-keysym)
(define-keysym :right-alt right-alt-keysym)
(define-keysym :left-super left-super-keysym)
(define-keysym :right-super right-super-keysym)
(define-keysym :left-hyper left-hyper-keysym)
(define-keysym :right-hyper right-hyper-keysym)

(define-keysym #\space 032)
(define-keysym #\! 033)
(define-keysym #\" 034)
(define-keysym #\# 035)
(define-keysym #\$ 036)
(define-keysym #\% 037)
(define-keysym #\& 038)
(define-keysym #\' 039)
(define-keysym #\( 040)
(define-keysym #\) 041)
(define-keysym #\* 042)
(define-keysym #\+ 043)
(define-keysym #\, 044)
(define-keysym #\- 045)
(define-keysym #\. 046)
(define-keysym #\/ 047)
(define-keysym #\0 048)
(define-keysym #\1 049)
(define-keysym #\2 050)
(define-keysym #\3 051)
(define-keysym #\4 052)
(define-keysym #\5 053)
(define-keysym #\6 054)
(define-keysym #\7 055)
(define-keysym #\8 056)
(define-keysym #\9 057)
(define-keysym #\: 058)
(define-keysym #\; 059)
(define-keysym #\< 060)
(define-keysym #\= 061)
(define-keysym #\> 062)
(define-keysym #\? 063)
(define-keysym #\@ 064)
(define-keysym #\A 065 :lower 097)
(define-keysym #\B 066 :lower 098)
(define-keysym #\C 067 :lower 099)
(define-keysym #\D 068 :lower 100)
(define-keysym #\E 069 :lower 101)
(define-keysym #\F 070 :lower 102)
(define-keysym #\G 071 :lower 103)
(define-keysym #\H 072 :lower 104)
(define-keysym #\I 073 :lower 105)
(define-keysym #\J 074 :lower 106)
(define-keysym #\K 075 :lower 107)
(define-keysym #\L 076 :lower 108)
(define-keysym #\M 077 :lower 109)
(define-keysym #\N 078 :lower 110)
(define-keysym #\O 079 :lower 111)
(define-keysym #\P 080 :lower 112)
(define-keysym #\Q 081 :lower 113)
(define-keysym #\R 082 :lower 114)
(define-keysym #\S 083 :lower 115)
(define-keysym #\T 084 :lower 116)
(define-keysym #\U 085 :lower 117)
(define-keysym #\V 086 :lower 118)
(define-keysym #\W 087 :lower 119)
(define-keysym #\X 088 :lower 120)
(define-keysym #\Y 089 :lower 121)
(define-keysym #\Z 090 :lower 122)
(define-keysym #\[ 091)
(define-keysym #\\ 092)
(define-keysym #\] 093)
(define-keysym #\^ 094)
(define-keysym #\_ 095)
(define-keysym #\` 096)
(define-keysym #\a 097)
(define-keysym #\b 098)
(define-keysym #\c 099)
(define-keysym #\d 100)
(define-keysym #\e 101)
(define-keysym #\f 102)
(define-keysym #\g 103)
(define-keysym #\h 104)
(define-keysym #\i 105)
(define-keysym #\j 106)
(define-keysym #\k 107)
(define-keysym #\l 108)
(define-keysym #\m 109)
(define-keysym #\n 110)
(define-keysym #\o 111)
(define-keysym #\p 112)
(define-keysym #\q 113)
(define-keysym #\r 114)
(define-keysym #\s 115)
(define-keysym #\t 116)
(define-keysym #\u 117)
(define-keysym #\v 118)
(define-keysym #\w 119)
(define-keysym #\x 120)
(define-keysym #\y 121)
(define-keysym #\z 122)
(define-keysym #\{ 123)
(define-keysym #\| 124)
(define-keysym #\} 125)
(define-keysym #\~ 126)
(progn   ;; Semi-standard characters
  (define-keysym #\rubout (keysym 255 255))	; :tty
  (define-keysym #\tab (keysym 255 009))	; :tty
  (define-keysym #\linefeed (keysym 255 010))	; :tty
  (define-keysym #\page (keysym 009 227))	; :special
  (define-keysym #\return (keysym 255 013))	; :tty
  (define-keysym #\backspace (keysym 255 008)))	; :tty

;; these keysym definitions are only correct if the underlying lisp's
;; definition of characters between 160 and 255 match latin1 exactly. If the
;; characters are in some way locale-dependent (as, I believe, in Allegro8) or
;; are treated as opaque without any notions of graphicness or case (as in
;; cmucl and openmcl) then defining these keysyms are either not useful or
;; wrong. -- CSR, 2006-03-14

;; NOTE 2026-01-25: includes non-BASE-CHARs.
(progn
  (do ((i 160 (+ i 1)))
      ((>= i 256))
    (if (or (<= #xc0 i #xd6)
            (<= #xd8 i #xde))
        (define-keysym (code-char i) i :lower (+ i 32))
        (define-keysym (code-char i) i))))

(define-keysym-names 
  ("RET" "Return")
  ("ESC" "Escape")
  ("TAB" "Tab")
  ("DEL" "BackSpace")
  ("SPC" "space")
  ("!" "exclam")
  ("\"" "quotedbl")
  ("$" "dollar")
  ("£" "sterling")
  ("%" "percent")
  ("&" "ampersand")
  ("'" "apostrophe")
  ("`" "grave")
  ("&" "ampersand")
  ("(" "parenleft")
  (")" "parenright")
  ("*" "asterisk")
  ("+" "plus")
  ("," "comma")
  ("-" "minus")
  ("." "period")
  ("/" "slash")
  (":" "colon")
  (";" "semicolon")
  ("<" "less")
  ("=" "equal")
  (">" "greater")
  ("?" "question")
  ("@" "at")
  ("[" "bracketleft")
  ("\\" "backslash")
  ("]" "bracketright")
  ("^" "asciicircum")
  ("_" "underscore")
  ("#" "numbersign")
  ("{" "braceleft")
  ("|" "bar")
  ("}" "braceright")
  ("~" "asciitilde")
  ("«" "guillemotleft")
  ("»" "guillemotright")
  ("À" "Agrave")
  ("à" "agrave")
  ("Ç" "Ccedilla")
  ("ç" "ccedilla")
  ("É" "Eacute")
  ("é" "eacute")
  ("È" "Egrave")
  ("è" "egrave")
  ("Ê" "Ecircumflex")
  ("ê" "ecircumflex"))

(defun keysym-downcase (keysym)
  (declare (keysym keysym))
  (or (let ((val (gethash keysym *keysym-character-table*)))
        (char-map-lower val))
      keysym))

(defun keysym-cased-p (keysym)
  ;; Returns T if keysym has a lowercase equivalent.
  (declare (keysym keysym))
  (declare (values (or null keysym)))
  (let ((translations (gethash keysym *keysym-character-table*)))
    (and translations
         (third (first translations)))))

(defun keysyms-from-character (char)
  "Given a character, return a list of all matching keysyms."
  (collecting
    (maphash #'(lambda (keysym mappings)
                 (dolist (mapping mappings)
                   (when (eql mapping char)
                     (collect keysym))))
             *keysym-character-table*)))

;;; Key
;; Note that the XLIB 'modifier-mask' type is (unsigned-byte 8), but only
;; contains CONTROL META ALT and MOD1-5. In this bitfield we preserve the
;; Symbolics-era keysyms (SUPER HYPER META), and disregard the
;; SCROLL-LOCK. Our WM is responsible for handling scroll-lock modifiers.
(eval-always
  (define-bitfield keymod 
    (shift boolean) 
    (control boolean) 
    (meta boolean)
    (alt boolean)
    (hyper boolean)
    (super boolean)
    (altgr boolean)
    (numlock boolean))
  (defstruct (key (:constructor %make-key)) sym (mod 0 :type keymod))
  (defun make-key (&rest args)
    (let ((sym (getf args :sym)))
      (remf args :sym)
      (%make-key :sym sym :mod (or (getf args :mod) 
                                   (apply 'make-keymod args))))))

(defgeneric key (self)
  (:documentation "Return the KEY associated with SELF."))
(defgeneric (setf key) (new self))

(macrolet ((defkfn (name)
             (with-gensyms (key mod val)
               `(prog1 (defun ,(symbolicate "KEY-" name) (,key)
                         (declare (key ,key) (optimize (speed 3) (safety 0)))
                         (lety ((,mod (key-mod ,key) :type keymod))
                           (,(symbolicate "KEYMOD-" name) ,mod)))
                  (defun (setf ,(symbolicate "KEYMOD-" name)) (,val ,key)
                    (declare (keymod ,key) (boolean ,val))
                    (cond
                      ((and ,val (not (,(symbolicate "KEYMOD-" name) ,key)))
                       (incf ,key (make-keymod ,(keywordicate name) t)))
                      ((and (,(symbolicate "KEYMOD-" name) ,key) (not ,val))
                       (decf ,key (make-keymod ,(keywordicate name) t)))
                      (t ,key)))
                  (defun (setf ,(symbolicate "KEY-" name)) (,val ,key)
                    (declare (key ,key) (boolean ,val))
                    (lety ((,mod (key-mod ,key) :type keymod))
                      (setf (key-mod ,key) (+ ,mod (make-keymod ,(keywordicate name) ,val)))))))))
  (defkfn control)
  (defkfn meta)
  (defkfn alt)
  (defkfn shift)
  (defkfn hyper)
  (defkfn super)
  (defkfn altgr)
  (defkfn numlock))

(definline key-mods-p (key) (not (zerop (key-mod key))))

(defun altgr-key (key)
  (declare (key key))
  (with-slots (mod) key
    (setf (key-mod key) (+ mod #.(make-keymod :altgr t)))
    key))

(defun numlock-key (key)
  (declare (key key))
  (with-slots (mod) key
    (setf (key-mod key) (+ mod #.(make-keymod :numlock t)))
    key))

(defstruct keybind key cmd)
(deftype keymap () '(vector keybind))
(definline keymap-p (obj) (typep obj 'keymap))
(defun keymap (&optional name) (if name (gethash name *keymaps*) *keymap*))
(defun (setf keymap) (val name) (setf (gethash name *keymaps*) val))

(eval-always
(with-memoization ()
  (memoizing
   (defun parse-mods (mods end)
     "MODS is a sequence of <MOD CHAR> #\- pairs which is parsed into a KEYMOD."
     (unless (evenp end)
       (error 'kbd-parse-error :item mods
                               :reason "Did you forget to separate modifier characters with '-'?"))
     (apply 'make-keymod
            (loop for i from 0 below end by 2
                  when (char/= (char mods (1+ i)) #\-)
                  do (error 'kbd-parse-error :item mods)
                  nconc (case (char mods i)
                          (#\M (list :meta t))
                          (#\A (list :alt t))
                          (#\C (list :control t))
                          (#\H (list :hyper t))
                          (#\s (list :super t))
                          (#\S (list :shift t))
                          (t (error 'kbd-parse-error 
                                    :item mods
                                    :reason (format nil "Unknown modifer character ~A" (char mods i)))))))))

  (memoizing
   (defun print-key-mods (key)
     (concatenate 'string
                  (when (key-control key) "C-")
                  (when (key-meta key) "M-")
                  (when (key-alt key) "A-")
                  (when (key-shift key) "S-")
                  (when (key-super key) "s-")
                  (when (key-hyper key) "H-"))))
  (memoizing
   (defun print-key (key)
     (format nil "~a~a"
             (print-key-mods key)
             (name-from-keysym (key-sym key)))))
  (defun print-key-seq (seq)
    (format nil "*~{~a~^ ~}" (mapcar 'print-key seq)))
    (memoizing
     (defun parse-key (string)
       "Parse STRING and return a KEY structure. Raise an error of type
KBD-PARSE-ERROR if the key failed to parse."
       (let* ((p (when (> (length string) 2)
                   (position #\- string :from-end t :end (- (length string) 1))))
              (mods (parse-mods string (if p (1+ p) 0)))
              (keysym (keysym-from-name (subseq string (if p (1+ p) 0)))))
         (if keysym
             (make-key :sym keysym :mod mods)
             (error 'kbd-parse-error :item string)))))
  (memoizing
   (defun parse-key-seq (keys)
     "KEYS is a key sequence. Parse it and return the list of keys."
     (mapcar 'parse-key (split-whitespace keys))))
  (memoizing
   (defun kbd (keys)
     "This compiles a key string into a key structure used by
`define-key', `set-prefix-key' and others."
     (let ((seq (parse-key-seq keys)))
       (values (car seq) (cdr seq)))))))

(eval-always
  (definline key= (key1 key2)
    (and (= (the keysym (key-sym key1)) (the keysym (key-sym key2)))
         (= (the keymod (key-mod key1)) (the keymod (key-mod key2))))))

(define-constant +unbound-key+ (make-key :sym 0 :mod 255) :test 'key=)
(define-constant +default-escape-key+ (make-key :sym 103 :mod 2) :test 'key=) ;; (parse-key "C-g")

(defun key-eq (key1 key2)
  (or (and (typep key1 'key) (typep key2 'key) (key= key1 key2))
      (eql key1 key2)))

(defun find-key (key map)
  ;; TODO 2026-02-07: designate an actual default key not T
  ;; (if (eql key t)
  ;; (find t map :key 'keybind-key :test 'key-eq)
  (find key map :key 'keybind-key :test 'key-eq))

;; XXX: define-key needs to be fixed to handle a list of keys
(defun define-key (map key cmd)
  "Add a keybinding mapping for the key, KEY to the command,
COMMAND, in the specified keymap. If COMMAND is nil, remove an
existing binding. For example,

Example: (define-key some-keymap (kbd \"C-z\") some-cmd-or-object)"
  (declare (keymap map) (type (or key (eql t)) key))
  (let ((binding (find-key key map)))
    (prog1
        (cond 
          (cmd
           (when binding (setf map (delete binding map)))
           (vector-push-extend (make-keybind :key key :cmd cmd) map))
          (t (setf map (delete binding map))))
      (funcall *keymap-hook* :define map))))

(definline sparse-keymap ()
  (make-array 0 :element-type 'keybind :fill-pointer t))

(defun lookup-cmd (keymap cmd)
  "Return a list of keys in KEYMAP that are bound to CMD."
  (loop for i in keymap
        when (equal cmd (keybind-cmd i))
        collect (keybind-key i)))

(defun lookup-key (keymap key &optional default)
  (when-let ((kb (or (find-key key keymap) (when default (find-key t keymap)))))
    (keybind-cmd kb)))

(defmethod copy ((from key) (to key))
  (setf (key-sym to) (key-sym from)
        (key-mod to) (key-mod from))
  to)

(defun keymap-symbol-p (x)
  (and (symbolp x)
       (boundp x)
       (keymap-p (symbol-value x))))

(defun keymap-or-keymap-symbol-p (x)
  (or (keymap-p x)
      (keymap-symbol-p x)))

(defun lookup-key-sequence (map key-seq)
  "Return the command bound to KEY-SEQ in keymap MAP."
  (when (keymap-symbol-p map)
    (setf map (symbol-value map)))
  (check-type map keymap)
  (let* ((key (car key-seq))
         (cmd (lookup-key map key)))
    (cond ((null (cdr key-seq))
           cmd)
          (cmd
           (if (keymap-or-keymap-symbol-p cmd)
               (lookup-key-sequence cmd (cdr key-seq))
               cmd))
          (t nil))))

;; TODO: we don't want lists here - fix wm/xlib
(defun deref-keymaps (maps)
  (map 'list
       (lambda (m)
         (if (keymap-symbol-p m)
             (symbol-value m)
             m))
       maps))

(defun search-keymap (command keymap &key (test 'equal))
  "Search the keymap for the specified binding. Return the key
sequences that run binding."
  (labels ((search-it (cmd kmap key-seq)
             (when (keymap-symbol-p kmap)
               (setf kmap (symbol-value kmap)))
             (check-type kmap keymap)
             (loop for i across kmap
                   if (funcall test (keybind-cmd i) cmd)
                   collect (cons (keybind-key i) key-seq)
                   else if (keymap-or-keymap-symbol-p (keybind-cmd i))
                   append (search-it cmd (keybind-cmd i) (cons (keybind-key i) key-seq)))))
    (mapcar 'reverse (search-it command keymap nil))))

(defmacro define-keymap (name (&optional parent modify) &body bindings) ;; full:t=generate charvec,nil=sparse-keymap
  "Define a new KEYMAP designated by NAME.

PARENT is the keymap to inherit from. If NAME is not a KEYWORD it is
interpreted as the name of a KEYMAP-SYMBOL."
  (let ((km (or (when modify (keymap name)) '(sparse-keymap)))
        (n (if (keywordp name) `(keymap ,name) name)))
    (when parent (copy (keymap parent) km))
    (unless (or modify (and (keymap-symbol-p n) (not (sequence:emptyp (symbol-value n)))))
      (with-gensyms (k)
        `(let ((,k ,km))
           ,@(loop for i = bindings then (cddr i) while i
                   collect `(define-key ,k ,(first i) ,(second i)))
           (setf ,n ,k))))))

;;; Keyboard
(defstruct keyboard 
  path 
  (sap nil :type (or null (alien (* libevdev)))) ;; device
  (state nil)
  (compose-state nil)
  (keymap nil))

(defaccessor sap ((self keyboard)) (keyboard-sap self))

(defun evdev-bit-p (array bit)
  "Array elements should be unsigned-long."
  (let ((idx (/ bit +long-bit+)))
    ;; the literal 1 here is 1LL in C - there is potential to overflow a
    ;; singled long.
    (logand (aref array idx) (ash 1 (mod bit +long-bit+)))))

(defun new-device-from-path (path)
  ;; opening FD may fail if the user does not have read permissions. When
  ;; ERROR is non-nil (the default) this signals an error, else we return nil.
  (with-fd (fd path :flags sb-posix:o-rdonly :close nil)
    (sb-alien:with-alien ((dev (* evdev::libevdev)))
      (let ((ret (evdev:libevdev-new-from-fd fd (sb-alien:addr dev))))
        (if (minusp ret)
            (simple-kbd-error (sb-unix::strerror (abs ret)))
            dev)))))

;; (keysym-name 400) ; "0x00000190"
;; evdev::+ev-cnt+ evdev::+key-cnt+

(defun keyboard-device-p (path)
  "Read some input on device at PATH returning T if it appears to be a keyboard
device."
  (with-open-file (st path :element-type 'octet)
    (let ((evbits (make-array evdev::+ev-cnt+))
          (keybits (make-array evdev::+key-cnt+)))
      ;; (sb-posix:ioctl (fd path)
      (read-sequence evbits st)
      (read-sequence keybits st)
      ;; (cons evbits keybits)
      (loop for i from evdev::+key-reserved+ upto evdev::+key-min-interesting+
            when (evdev-bit-p keybits i)
            return t))))

(defun make-keyboard-from-dev (dev &rest args)
  "Return a KEYBOARD given a device, keymap, and compose table. Keyword argument
ERROR when non-nil (the default) causes an error to be signaled if the device
can't be opened, else returns nil."
  (apply 'make-keyboard :sap dev args))

(defun get-keyboards (&optional (dir "/dev/input/"))
  (let ((devices (directory-files dir))
        ret)
    (dolist (dev devices ret)
      (handler-case (push (make-keyboard-from-dev (new-device-from-path dev) :path dev)
                          ret)
        (sb-posix:syscall-error () nil)
        (simple-kbd-error () nil)))))

;; (xkb::xkb-consumed-mode :xkb)

(defun print-device-input-info (path)
  (when-let ((dev (new-device-from-path path)))
    (pprint 
     (list (evdev::libevdev-get-name dev) 
           (evdev::libevdev-get-id-bustype dev) 
           (evdev::libevdev-get-id-vendor dev)
           (evdev::libevdev-get-id-product dev)))))

(defun device-read-event (dev)
  (declare (optimize (speed 3) (safety 0)))
  (with-alien ((ev evdev/input:input-event))
    (when (evdev::libevdev-has-event-pending dev)
      (println "has event pending")
      (evdev::libevdev-next-event dev (libevdev-read-flag :normal) (addr ev)))
    (with-alien-slots ((* time) type (code evdev/input::code) (value evdev/input::value)) ev
      (values 
       (sb-posix::alien-timeval-sec time)
       (the fixnum (* 1000 (sb-posix::alien-timeval-usec time)))
       (evdev::libevdev-event-type-get-name type) 
       (evdev::libevdev-event-code-get-name type code) 
       value))))

(defun device-read-events (dev count)
  (let (ret)
    (dotimes (i count ret)
      (multiple-value-bind (s ms type code val) (device-read-event dev)
        (push (list type code val (cons s ms)) ret)))))

(defconfig kbd-config ()
  ((device)
   (prefix-key :initform (kbd "s-x") :accessor prefix-key)
   (escape-key :initform (kbd "C-g") :accessor escape-key)
   (keymaps :initform nil :accessor keymaps)))

(defmethod make-config ((self (eql :kbd)) &rest args) (apply 'make-instance 'kbd-config args))

(defmethod load-ast ((self kbd-config))
  (with-slots (ast) self
    (sb-int:doplist (k v) ast
      (when-let ((s (find-symbol (format nil "~A" k) :io/kbd)))
        (unless (null v)
          (setf v
                (case k
                  ((or :escape-key :prefix-key) (parse-key v))
                  (t v)))
          (setf (slot-value self s) v))))
      (unless *keep-ast* (setf (ast self) nil))))

(defmethod load-config ((self (eql :kbd)) (from pathname) &key)
  (let ((c (make-config :kbd)))
    (with-safe-io-syntax (:io/kbd)
      (read-ast c from)
      (load-ast c))
    (setf (ast c) nil)
    c))

(defmethod load-config ((self (eql :kbd)) (from list) &key)
  (let ((c (make-config :kbd)))
    (sb-int:doplist (k v) from
      (when-let ((s (find-symbol (format nil "~A" k) :io/kbd)))
        (unless (null v)
          (case k
            ((or :escape-key :prefix-key) (setf (slot-value c s) (parse-key v)))))))
    c))

(defmethod init ((self (eql :kbd)) &key (input "/dev/input/") keysyms)
  (load-kbd-libs)
  (when keysyms (load-xkb-keysyms-file keysyms))
  (when input (setq *keyboards* (get-keyboards input))))
