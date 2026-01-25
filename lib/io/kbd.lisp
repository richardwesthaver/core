;;; kbd.lisp --- Keyboard-based IO

;; Keyboard-like devices and input

;;; Commentary:

;; refs:

;; - https://www.kernel.org/doc/Documentation/input/event-codes.txt

;; - https://github.com/xkbcommon/libxkbcommon/blob/master/tools/interactive-evdev.c

;; - https://gitlab.freedesktop.org/libevdev/libevdev/-/tree/master/tools

;;; Code:
(in-package :io/kbd)

(defun load-kbd-libs ()
  (load-xkbcommon)
  (load-evdev))

;;; Vars
(defconstant +evdev-offset+ 8)
(defconstant +long-bit+ (sb-alien:alien-size sb-alien:unsigned-long))
(defparameter *keyboards* nil)
(defvar *keysym-sets* nil
  "Alist of (NAME FIRST LAST).")
(defvar *character-keysym-table* (make-hash-table :test 'eql)
  "Table mapping Characters to KEYSYMs.")
(defvar *keysym-name-table* (make-hash-table))
(defvar *name-keysym-table* (make-hash-table :test #'equal))
(defvar *dead-keysym-name-table* (make-hash-table))

;;; Conditions
(define-condition kbd-error (error) ())
(deferror simple-kbd-error (simple-error kbd-error) () (:auto t))
;;; Keysym
(deftype keysym () '(unsigned-byte 32))

(definline keysym-name (code)
  (with-alien ((str (* unsigned-char) (make-alien unsigned-char 11)))
    (xkb::xkb-keysym-get-name code str 11)
    (cast str c-string)))

(definline keysym-code-name (code)
  (gethash code *keysym-name-table*))

(definline keysym-name-code (name)
  (gethash name *name-keysym-table*))

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

(defun keysym (key &rest bytes)
  "Build a 32-bit keysym. KEY is an integer or character and BYTES optionally
fill in the lower bytes."
  (declare (dynamic-extent bytes))
    (etypecase key
      (keysym
       (dolist (b bytes key) (setq key (+ (ash key 8) b))))
      (character
       (or (gethash key *character-keysym-table*)
           (error "~s isn't a keysym" key)))))

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

(defmacro define-keysym (obj keysym &key lower translate modifiers mask))

#+nil
(progn
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
  (define-keysym #\A 065 :lowercase 097)
  (define-keysym #\B 066 :lowercase 098)
  (define-keysym #\C 067 :lowercase 099)
  (define-keysym #\D 068 :lowercase 100)
  (define-keysym #\E 069 :lowercase 101)
  (define-keysym #\F 070 :lowercase 102)
  (define-keysym #\G 071 :lowercase 103)
  (define-keysym #\H 072 :lowercase 104)
  (define-keysym #\I 073 :lowercase 105)
  (define-keysym #\J 074 :lowercase 106)
  (define-keysym #\K 075 :lowercase 107)
  (define-keysym #\L 076 :lowercase 108)
  (define-keysym #\M 077 :lowercase 109)
  (define-keysym #\N 078 :lowercase 110)
  (define-keysym #\O 079 :lowercase 111)
  (define-keysym #\P 080 :lowercase 112)
  (define-keysym #\Q 081 :lowercase 113)
  (define-keysym #\R 082 :lowercase 114)
  (define-keysym #\S 083 :lowercase 115)
  (define-keysym #\T 084 :lowercase 116)
  (define-keysym #\U 085 :lowercase 117)
  (define-keysym #\V 086 :lowercase 118)
  (define-keysym #\W 087 :lowercase 119)
  (define-keysym #\X 088 :lowercase 120)
  (define-keysym #\Y 089 :lowercase 121)
  (define-keysym #\Z 090 :lowercase 122)
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
  ;; cmucl and openmcl) then defining these keysyms is either not useful or
  ;; wrong. -- CSR, 2006-03-14
  (progn
    (do ((i 160 (+ i 1)))
        ((>= i 256))
      (if (or (<= #xc0 i #xd6)
              (<= #xd8 i #xde))
          (define-keysym (code-char i) i :lowercase (+ i 32))
          (define-keysym (code-char i) i)))))

;;; Objects
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
      (handler-case
          (progn
            (print-device-input-info dev)
            (push (make-keyboard-from-dev (new-device-from-path dev) :path dev)
                  ret))
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
        
(defmethod init ((self (eql :kbd)) &key (directory "/dev/input/"))
  (load-kbd-libs)
  (when directory (setq *keyboards* (get-keyboards directory))))
