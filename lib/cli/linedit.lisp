;;; linedit.lisp --- Sharplispers Line Editor

;; See https://github.com/sharplispers/linedit

;;; Code:
(in-package :linedit)

;;; Vars
(defvar *history* nil)
(defvar *killring* nil)
(declaim (type simple-string *word-delimiters*))
(defparameter *word-delimiters* "()[]{}',` \"")
(defconstant +linedit-ok+ 0)
(defconstant +linedit-not-atty+ 1)
(defconstant +linedit-memory-error+ 2)
(defconstant +linedit-tcgetattr-error+ 3)
(defconstant +linedit-tcsetattr-error+ 4)
(defconstant +linedit-attr-error+ 5)
(defconstant +linedit-no-attr-error+ 6)
(defvar *terminal-translations* (make-hash-table :test #'equalp))

;; TODO 2026-01-31: use command protocol (REPL)?
(defvar *cmds* (make-hash-table :test #'equalp))

;;; Utils
(defmacro ensure (symbol expr)
  `(or ,symbol (setf ,symbol ,expr)))

(defun word-delimiter-p (char)
  (declare (simple-string *word-delimiters*)
	   (character char))
  (find char *word-delimiters*))

(defun whitespacep (char)
  (member char '(#\space #\newline #\tab #\return #\page)))

(defun at-delimiter-p (string index)
  (and (< index (length string))
       (word-delimiter-p (char string index))))

;;;; QUOTES

;; FIXME: should checking for #\", "\"", et cetera.
(defun quoted-p (string index)
  (let ((quoted-p nil))
    (dotimes (n (min index (length string)) quoted-p)
      (when (eql (schar string n) #\")
	(setf quoted-p (not quoted-p))))))

(defun find-open-quote (string index)
  (when (quoted-p string index)
    (loop for n from (1- index) downto 0
	  when (eql (schar string n) #\") return n)))

(defun find-close-quote (string index)
  (when (quoted-p string index)
    (loop for n from (1+ index) below (length string)
	  when (eql (schar string n) #\") return n)))

;; (defun dwim-match-quotes (string index))

;; (defun dwim-mark-quotes (string index &key pre post))

(defun yes-or-no (control &rest args)
  "Like Y-OR-N-P, but using linedit functionality."
  ;; Don't save the query response.
  (let ((*history* nil)
        (*killring* nil))
    (declare (ignorable *history* *killring*))
    (loop
      (let ((result (linedit :prompt (format nil "~? (y or n) " control args))))
        (cond
          ((zerop (length result)))
          ((char-equal (elt result 0) #\y)
           (return-from yes-or-no t))
          ((char-equal (elt result 0) #\n)
           (return-from yes-or-no nil)))
        (format *terminal-io* "Please type \"y\" for yes or \"n\" for no.~%")
        (finish-output *terminal-io*)))))

(defun eof-handler (lisp-name quit-fn)
  (handler-case
      (cond ((yes-or-no "Really quit ~A?" lisp-name)
             (fresh-line)
             (funcall quit-fn))
            (t
             (return-from eof-handler "#.''end-of-file")))
    (end-of-file ()
      (fresh-line)
      (funcall quit-fn))))

;;;; PARENS

;; FIXME: This is not the Right Way to do paren matching.
;; * use stack, not counting
;; * don't count #\( #\) &co
(defun after-close-p (string index)
  (and (array-in-bounds-p string (1- index))
       (find (schar string (1- index)) ")]}")))

(defun at-open-p (string index)
  (and (array-in-bounds-p string index)
       (find (schar string index) "([{")))

(defun paren-count-delta (char)
  (case char
    ((#\( #\[ #\{) -1)
    ((#\) #\] #\}) 1)
    (t 0)))

(defun find-open-paren (string index)
  (loop with count = 1
	for n from (1- index) downto 0
	do (incf count (paren-count-delta (schar string n)))
	when (zerop count) return n))

(defun find-close-paren (string index)
  (loop with count = -1
	for n from (1+ index) below (length string)
	do (incf count (paren-count-delta (schar string n)))
	when (zerop count) return n))

(defun dwim-match-parens (string index)
  (cond ((after-close-p string index)
	 (values (find-open-paren string (1- index)) (1- index)))
	((at-open-p string index)
	 (values index (find-close-paren string index)))
	(t 
	 (values nil nil))))

(defun dwim-mark-parens (string index &key pre post)
  (multiple-value-bind (open close) (dwim-match-parens string index)
    (values 
     (if (and open close)
	 (concatenate 'simple-string
                      (subseq string 0 open)
		      pre
		      (string (schar string open))
		      post
		      (subseq string (1+ open) close)
		      pre
		      (string (schar string close))
		      post
		      (if (> (length string) (1+ close))
		          (subseq string (1+ close))
		          ""))
	 string)
     open)))

(defparameter *default-columns* 80)
(defparameter *default-lines* 24)
(defparameter *highlight-color* :magenta
  "Color to use for highlighting parentheses. NIL is the current foreground
color bolded, other options are terminal colors :BLACK, :RED, :GREEN, :YELLOW,
:BLUE, :MAGENTA, :CYAN, and :WHITE.")

(defclass backend ()
  ((ready-p :accessor backend-ready-p :initform nil)
   (translations :reader backend-translations)
   (start :initform 0 :accessor get-start)))

(defmacro with-backend (backend &body forms)
  (with-gensyms (an-error)
    `(let ((,an-error nil))
       (unwind-protect
	    (handler-case (progn
			    (backend-init ,backend)
			    ,@forms)
	      (error (e)
		(setf ,an-error e)))
	 (backend-close ,backend)
	 (std:awhen ,an-error
	   (error std:it))))))

(defmacro without-backend (backend &body forms)
  `(unwind-protect
	(progn
	  (backend-close ,backend)
	  ,@forms)
     (backend-init ,backend)))

(defgeneric display (backend &key prompt line point &allow-other-keys))

;;; Terminal Glue
(eval-always
  (let (attr)
    (defun c-terminal-init ()
      (if (zerop (isatty 0))
          (return-from c-terminal-init +linedit-not-atty+))
      ;; Save current terminal state in attr
      (when attr
        (warn "bad linedit attr: ~A" attr)
        (return-from c-terminal-init +linedit-attr-error+))
      (setf attr (std::foreign-alloc 'sb-posix::alien-termios))
      (when (minusp (std::tcgetattr* 0 attr))
        (return-from c-terminal-init +linedit-tcgetattr-error+))
      ;; Enter keyboard input mode
      (sb-alien:with-alien ((tmp sb-posix::alien-termios))
        (when (minusp (tcgetattr* 0 (sb-alien:addr tmp)))
          (return-from c-terminal-init +linedit-tcgetattr-error+))
        (cfmakeraw (sb-alien:addr tmp))
        (with-alien-slots (sb-posix::oflag) tmp
          (setf sb-posix::oflag (logior sb-posix::oflag sb-posix::opost)))
        (if (minusp (tcsetattr* 0 sb-posix::tcsaflush (sb-alien:addr tmp)))
            +linedit-tcsetattr-error+))
      +linedit-ok+)
    (defun c-terminal-close ()
      ;; Restore saved terminal state from attr
      (when (null attr)
        (warn "missing linedit attr on close")
        (return-from c-terminal-close +linedit-no-attr-error+))
      (when (zerop (isatty 0))
        (return-from c-terminal-close +linedit-not-atty+))
      (when (minusp (tcsetattr* 0 sb-posix::tcsanow attr))
        (return-from c-terminal-close +linedit-tcsetattr-error+))
      (std:foreign-free attr)
      (setf attr nil)
      +linedit-ok+)))

(defun c-terminal-winsize (def side side-env)
  (if (boundp 'std::+tiocgwinsz+)
      (sb-alien:with-alien ((size winsize))
        (and (zerop (ioctl 0 std::+tiocgwinsz+ (sb-alien:cast size (* t))))
             (sb-alien:slot size side)))
      (aif (getenv side-env)
           (parse-integer it)
           def)))

(defun c-terminal-lines (def)
  (c-terminal-winsize def 'std/os::row "LINES"))

(defun c-terminal-columns (def)
  (c-terminal-winsize def 'std/os::col "COLUMNS"))

;;; Terminal Translations
(defmacro deftrans (name &rest chords)
  `(dolist (chord ',chords)
     (let ((old (gethash chord *terminal-translations*)))
       (when (and old (not (equal old ,name)))
	 (warn "Overriding old translation ~S for ~S with ~S." old chord ,name)))
     (setf (gethash chord *terminal-translations*) ,name)))

(deftrans "C-Space" 0)
(deftrans "C-A" 1)
(deftrans "C-B" 2)
(deftrans "C-C" 3)
(deftrans "C-D" 4)
(deftrans "C-E" 5)
(deftrans "C-F" 6)
(deftrans "C-G" 7)
(deftrans "C-Backspace" 8)
(deftrans "Tab" 9)
(deftrans "C-K" 11)
(deftrans "C-L" 12)
(deftrans "Return" 10 13) ;; Newline and return
(deftrans "C-N" 14)
(deftrans "C-O" 15)
(deftrans "C-P" 16)
(deftrans "C-Q" 17)
(deftrans "C-R" 18)
(deftrans "C-S" 19)
(deftrans "C-T" 20)
(deftrans "C-U" 21)
(deftrans "C-V" 22)
(deftrans "C-W" 23)
(deftrans "C-X" 24)
(deftrans "C-Y" 25)
(deftrans "C-Z" 26)
(deftrans "C--" 31)
(deftrans "Backspace" 127)

(deftrans "M-A" (#\Esc #\A) 225)
(deftrans "M-B" (#\Esc #\B) 226)
(deftrans "M-C" (#\Esc #\C) 227)
(deftrans "M-D" (#\Esc #\D) 228)
(deftrans "M-E" (#\Esc #\E) 229)
(deftrans "M-F" (#\Esc #\F) 230)
(deftrans "M-G" (#\Esc #\G) 231)
(deftrans "M-H" (#\Esc #\H) 232)
(deftrans "M-I" (#\Esc #\I) 233)
(deftrans "M-J" (#\Esc #\J) 234)
(deftrans "M-K" (#\Esc #\K) 235)
(deftrans "M-L" (#\Esc #\L) 236)
(deftrans "M-M" (#\Esc #\M) 237)
(deftrans "M-N" (#\Esc #\N) 238)
(deftrans "M-O" (#\Esc #\O) 239)
(deftrans "M-P" (#\Esc #\P) 240)
(deftrans "M-Q" (#\Esc #\Q) 241)
(deftrans "M-R" (#\Esc #\R) 242)
(deftrans "M-S" (#\Esc #\S) 243)
(deftrans "M-T" (#\Esc #\T) 244)
(deftrans "M-U" (#\Esc #\U) 245)
(deftrans "M-V" (#\Esc #\V) 246)
(deftrans "M-W" (#\Esc #\W) 247)
(deftrans "M-X" (#\Esc #\X) 248)
(deftrans "M-Y" (#\Esc #\Y) 249)
(deftrans "M-Z" (#\Esc #\Z) 250)
(deftrans "M-0" (#\Esc #\0) 176)
(deftrans "M-1" (#\Esc #\1) 177)
(deftrans "M-2" (#\Esc #\2) 178)
(deftrans "M-3" (#\Esc #\3) 179)
(deftrans "M-4" (#\Esc #\4) 180)
(deftrans "M-5" (#\Esc #\5) 181)
(deftrans "M-6" (#\Esc #\6) 182)
(deftrans "M-7" (#\Esc #\7) 183)
(deftrans "M-8" (#\Esc #\8) 184)
(deftrans "M-9" (#\Esc #\9) 185)
(deftrans "M-Backspace" (#\Esc #\Rubout))

(deftrans "C-M-f" (#\Esc #\^F) 134)
(deftrans "C-M-b" (#\Esc #\^B) 130)
(deftrans "C-M-k" (#\Esc #\^K) 139)

(deftrans "Up-arrow"    (#\Esc #\[ #\A))
(deftrans "Down-arrow"  (#\Esc #\[ #\B))
(deftrans "Right-arrow" (#\Esc #\[ #\C))
(deftrans "Left-arrow"  (#\Esc #\[ #\D))
(deftrans "Insert"      (#\Esc #\[ #\2 #\~))
(deftrans "Delete"      (#\Esc #\[ #\3 #\~))
(deftrans "C-Delete"    (#\Esc #\[ #\3 #\^))
(deftrans "Page-up"     (#\Esc #\[ #\5 #\~))
(deftrans "Page-down"   (#\Esc #\[ #\6 #\~))
(deftrans "Home"        (#\Esc #\[ #\7 #\~) (#\Esc #\[ #\1 #\~) (#\Esc #\[ #\H))
(deftrans "End"         (#\Esc #\[ #\8 #\~) (#\Esc #\[ #\4 #\~) (#\Esc #\[ #\F))

(defclass terminal (backend)
  ((translations :initform *terminal-translations*)
   (dirty-p :initform t :accessor dirty-p)))

(defmethod backend-columns ((backend terminal))
  (let ((cols (c-terminal-columns *default-columns*)))
    (if (> cols 0)
        cols
        *default-columns*)))

(defmethod backend-lines ((backend terminal))
  (c-terminal-lines *default-lines*))

(defmacro invariant (condition)
  (with-unique-names (value)
    `(let ((,value ,condition))
       (unless ,value
         (let ((*print-pretty* nil))
           (error "Invariant ~S violated."
                  ',condition))))))

(defmethod backend-init ((backend terminal))
  (invariant (not (backend-ready-p backend)))
  (invariant (zerop (c-terminal-init)))
  (setf (backend-ready-p backend) t))

(defmethod backend-close ((backend terminal))
  (invariant (backend-ready-p backend))
  (invariant (zerop (c-terminal-close)))
  (setf (backend-ready-p backend) nil))

;;; FIXME: Use read-char-no-hang to detect pastes, and set an
;;; apropriate flag, or something.
(defmethod read-chord ((backend terminal))
  (assert (backend-ready-p backend))
  (flet ((read-open-chord ()
	   (do ((chars nil)
		(c #1=(read-char) #1#))
	       ((member c '(#\- #\~ #\$)) (nconc (nreverse chars) (list c)))
	     (push c chars))))
    (let ((chord
	    (acase (read-char)
	      (#\Esc
	       (cons it (acase (read-char)
			  (#\[ (cons
			        it
			        (let ((char (read-char)))
				  (if (digit-char-p char)
				      (cons char
					    (read-open-chord))
				      (list char)))))
			  (t (list it)))))
	      (t (if (graphic-char-p it)
		     it
		     (char-code it))))))
      (gethash chord
	       (backend-translations backend)
	       (if (characterp chord)
		   chord
		   (list 'untranslated chord))))))

;;; ASCII 7 should ring the terminal bell. This is hopefully marginally more
;;; robust than #\Bel -- some implementations might eg. call it #\Bell, which
;;; is unicode character in eg. SBCL.
(defconstant +terminal-bell+ (code-char 7))

(defmethod beep ((b terminal))
  (declare (ignore b))
  (and (write-char +terminal-bell+ *error-output*)
       (force-output *error-output*)))

(defmethod page ((backend terminal))
  (write-string "--more--")
  (force-output)
  (let ((q (read-chord backend)))
    (write-char #\Return)
    (not (equal #\q q))))

;;; FIXME: Explicit line-wrap needed
(defmethod print-in-columns ((backend terminal) list &key width)
  (let ((max-col (truncate (backend-columns backend) width))
	(col 0)
	(line 0)
	(pad nil))
    (newline backend)
    (dolist (item list)
      (incf col)
      ;; Padding
      (when pad
	(write-string pad)
	(setf pad nil))
      ;; Item
      (write-string item)
      ;; Maybe newline
      (cond ((= col max-col)
	     (newline backend)
	     (setf col 0)
	     (when (= (1+ (incf line)) (backend-lines backend))
	       (setf line 0)
	       (unless (page backend)
		 (return-from print-in-columns nil))))
	    (t 
	     (setf pad (make-string (- width (length item)) 
				    :initial-element #\space)))))
    ;; Optional newline
    (when pad
      (newline backend))))

(defmethod print-in-lines ((backend terminal) string)
  (newline backend)
  (do ((i 0 (1+ i))
       (lines 0))
      ((= i (length string)))
    (let ((c (schar string i)))
      (when (= lines (backend-lines backend))
	(setf lines 0)
	(unless (page backend)
	  (return-from print-in-lines nil)))
      (when (eql #\newline c)
	(incf lines))
      (write-char c)))
  (newline backend))

(defmethod newline ((backend terminal))
  (setf (dirty-p backend) t)
  (write-char #\newline)
  (write-char #\return)
  (force-output))

;;; Smart Terminal
(defclass smart-terminal (terminal)
  ((old-point :initform 0 :accessor old-point)
   (old-string :initform "" :accessor old-string)
   (old-markup :initform 0 :accessor old-markup)))

(defun set-column-address (n current)
  (cond ((< n current)
	 (loop repeat (- current n) 
	       do (tputs cursor-left)))
	((> n current)
	 (loop repeat (- n current) 
	       do (tputs cursor-right)))))

(defun smart-terminal-p ()
  (and cursor-up cursor-down clr-eos
       (or column-address (and cursor-left cursor-right))
       (or auto-right-margin enter-am-mode)))

(defmethod backend-init ((backend smart-terminal))
  (call-next-method)
  (when enter-am-mode
    (tputs enter-am-mode)))

(defun find-row (n columns)
  ;; 1+ includes point in row calculations
  (ceiling (1+ n) columns))

(defun find-col (n columns)
  (rem n columns))

(defun move-in-column (&key col vertical clear-to-eos current-col)
  (set-column-address col current-col)
  (if (plusp vertical)
      (loop repeat vertical do (tputs cursor-up))
      (loop repeat (abs vertical) do (tputs cursor-down)))
  (when clear-to-eos
    (tputs clr-eos)))

(defun fix-wraparound (start end columns)
  ;; If final character ended in the last column the point
  ;; will wrap around to the first column on the same line:
  ;; hence move down if so.
  (when (and (< start end) (zerop (find-col end columns)))
    (tputs cursor-down)))

(defun place-point (&key up col)
  (loop repeat up do (tputs cursor-up))
  (tputs column-address col))

(definline paren-style ()
  (concatenate
   'simple-string
   (when *highlight-color*
     (tparm
      set-a-foreground
      (or (position *highlight-color* '(:black :red :green :yellow :blue :magenta :cyan :white))
          (error "Unknown color: ~S" *highlight-color*))))
   enter-bold-mode))

(defmethod display ((backend smart-terminal) &key prompt line point markup)
  (let* (;; SBCL and CMUCL traditionally point *terminal-io* to /dev/tty,
         ;; and we do output on it assuming it goes to STDOUT. Binding
         ;; *terminal-io* is unportable, so do it only when needed.
         (*terminal-io* *standard-output*)
	 (columns (backend-columns backend))
	 (old-markup (old-markup backend))
	 (old-point (old-point backend))
	 (old-col (find-col old-point columns))
	 (old-row (find-row old-point columns))
	 (old (old-string backend))
	 (new (concatenate 'simple-string prompt line))
	 (end (length new))
	 (rows (find-row end columns)))
    (when (dirty-p backend)
      (setf old-markup 0
	    old-point 0
	    old-col 0
	    old-row 1))
    (multiple-value-bind (marked-line markup)
	(if markup
	    (dwim-mark-parens line point
			      :pre (paren-style)
			      :post exit-attribute-mode)
	    (values line point))
      (let* ((full (concatenate 'simple-string prompt marked-line))
	     (point (+ point (length prompt)))
	     (point-row (find-row point columns))
	     (point-col (find-col point columns))
	     (diff (mismatch new old))
	     (start (apply 'min (remove-if 'null (list old-point point markup old-markup diff end))))
	     (start-row (find-row start columns))
	     (start-col (find-col start columns)))
	;; (dbg "---~%")
	;; (dbg-values (subseq new start))
	;; (dbg-values rows point point-row point-col start start-row start-col
	;;             old-point old-row old-col end diff)
	(move-in-column
	 :col start-col
	 :vertical (- old-row start-row)
	 :clear-to-eos t
	 :current-col old-col)
	(write-string (subseq full start))
	(fix-wraparound start end columns)
	(move-in-column 
	 :col point-col
	 :vertical (- rows point-row)
	 :current-col (find-col end columns))
	;; Save state
	(setf	(old-string backend) new
		(old-markup backend) markup
		(old-point backend) point
		(dirty-p backend) nil)))
    (force-output *terminal-io*)))

;;; Dumb Terminal
(defclass dumb-terminal (terminal) ())

(defmethod display ((backend dumb-terminal) &key prompt line point &allow-other-keys)
  (let* ((string (concatenate 'simple-string prompt line))
	 (length (length string))
	 (point (+ point (length prompt)))
	 (columns (backend-columns backend)))
    (write-char #\return)
    (cond ((< (1+ point) columns) 
	   (write-string (subseq string 0 (min length columns)))
	   (when (< length columns)
	     (write-string (make-string (- columns length) :initial-element #\space)))
	   (write-char #\return)
	   (write-string (subseq string 0 point)))
	  (t
	   (write-string (subseq string (- (1+ point) columns) point))
	   (write-char #\return)
	   (write-string (subseq string (- (1+ point) columns) point)))))
  (force-output))

;;; Command Keys
(defmacro defcmd (command &optional action)
  (when action
    `(setf (gethash ,command *cmds*) ,action)))

(defmacro defcmd-prefix (cmd &rest cmds)
  "Define a prefix command on CMD which interprets the next sequence read with
READ-CHORD according to CMDS."
  (let ((tbl (make-hash-table :test 'equalp :size (length cmds))))
    (dolist (c cmds tbl)
      (destructuring-bind (key act) c
        (when act (setf (gethash key tbl) act))))
    `(setf (gethash ,cmd *cmds*) ,tbl)))

(defcmd-prefix "C-X" ("C-X" move-to-bol))
(defcmd "C-A" 'move-to-bol)
(defcmd "C-B" 'move-char-left)
(defcmd "C-C" 'interrupt-lisp)
(defcmd "C-D" 'delete-char-forwards-or-eof)
(defcmd "C-E" 'move-to-eol)
(defcmd "C-F" 'move-char-right)
(defcmd "C-G")
(defcmd "C-J")
(defcmd "C-K" 'kill-to-eol)
(defcmd "C-L")
(defcmd "C-N" 'history-next)
(defcmd "C-O" 'close-all-sexp)
(defcmd "C-P" 'history-previous)
(defcmd "C-Q")
(defcmd "C-R" 'search-history-backwards)
(defcmd "C-S" 'search-history-forwards)
(defcmd "C-T")
(defcmd "C-U" 'kill-to-bol)
(defcmd "C-V")
(defcmd "C-W" 'cut-region)
;; (defcmd "C-X")
(defcmd "C-Y" 'yank)
(defcmd "C-Z" 'stop-lisp)
(defcmd "C--" 'undo)

(defcmd "M-A" 'apropos-word)
(defcmd "M-B" 'move-word-backwards)
(defcmd "M-C")
(defcmd "M-D" 'delete-word-forwards)
(defcmd "M-E")
(defcmd "M-F" 'move-word-forwards)
(defcmd "M-G")
(defcmd "M-H" 'help)
(defcmd "M-I" 'describe-word)
(defcmd "M-J" 'inspect-word)
(defcmd "M-K")
(defcmd "M-L" 'downcase-word)
(defcmd "M-M")
(defcmd "M-N")
(defcmd "M-O")
(defcmd "M-P")
(defcmd "M-Q")
(defcmd "M-R")
(defcmd "M-S")
(defcmd "M-T")
(defcmd "M-U" 'upcase-word)
(defcmd "M-V")
(defcmd "M-W" 'copy-region)
(defcmd "M-X")
(defcmd "M-Y" 'yank-cycle)
(defcmd "M-Z")
(defcmd "M-1")
(defcmd "M-2")
(defcmd "M-3")
(defcmd "M-4")
(defcmd "M-5")
(defcmd "M-6")
(defcmd "M-7")
(defcmd "M-8")
(defcmd "M-9")
(defcmd "M-0")

(defcmd "C-M-b" 'backward-sexp)
(defcmd "C-M-f" 'forward-sexp)
(defcmd "C-M-k" 'kill-sexp)

(defcmd "M-Backspace" 'delete-word-backwards)

(defcmd "C-Space" 'set-mark)
(defcmd "C-Backspace" 'delete-word-backwards)

(defcmd "Tab" 'complete)
(defcmd "Backspace" 'delete-char-backwards)
(defcmd "Return" 'finish-input)

(defcmd "Up-arrow" 'history-previous)
(defcmd "Down-arrow" 'history-next)
(defcmd "Right-arrow" 'move-char-right)
(defcmd "Left-arrow" 'move-char-left)
(defcmd "Insert" 'toggle-insert)
(defcmd "Delete" 'delete-char-forwards)
(defcmd "C-Delete")
(defcmd "Page-up")
(defcmd "Page-down")
(defcmd "Home" 'move-to-bol)
(defcmd "End" 'move-to-eol)

(defclass terminal-editor (editor)
  ((commands :reader editor-commands
             :initform *cmds*
             :initarg :commands)
   (completer :accessor editor-completer
              :initform 'lisp-complete
              :initarg :complete)
   (history :accessor editor-history)
   (killring :accessor editor-killring :type text-buffer)
   (prompt :accessor editor-prompt
           :initform ""
           :initarg :prompt)
   (yank :accessor editor-yank
         :initform nil)
   (last-yank :accessor editor-last-yank
              :initform nil)
   (insert :accessor editor-insert-mode
           :initform t
           :initarg :insert-mode)
   (mark :accessor editor-mark
         :initform nil)))

(defmethod initialize-instance :after ((editor terminal-editor) &rest initargs &key history killring completions)
  (declare (ignorable initargs))
  (let ((history (ensure-buffer (or history *history*))))
    (unless *history*
      (setf *history* history))
    (setf (editor-history editor) history))
  (let ((killring (ensure-buffer (or killring *killring*))))
    (unless *killring*
      (setf *killring* killring))
    (setf (editor-killring editor) killring))
  (when completions (setf (editor-completer editor) (make-list-completer completions)))
  (save-state editor))

(defclass smart-editor (terminal-editor smart-terminal) ())
(defclass dumb-editor (terminal-editor dumb-terminal) ())

(defvar *announce* nil)
(defvar *linedit-spec* nil)
(defvar *version* "0.1.2-cc")

(defun make-editor (&rest args)
  (set-terminal)
  (let* ((type (if (smart-terminal-p)
                   'smart-editor
                   'dumb-editor))
         (spec (list *version* type)))
    (when *announce*
      (unless (equal *linedit-spec* spec)
        (format t "~&Linedit version ~A, ~A mode, ESC-h for help.~%"
                *version*
                (if (eq 'smart-editor type)
                    "smart"
                    "dumb"))))
        (setf *linedit-spec* spec)
    (apply 'make-instance type args)))

(defvar *aux-prompt* nil)

(defun redraw-line (editor &key markup)
  (display editor
	   :prompt (concatenate 'simple-string (editor-prompt editor) *aux-prompt*)
	   :line (get-string editor)
	   :point (get-point editor)
	   :markup markup))

(defvar *last-command* nil)

(defun next-chord (editor)
  (redraw-line editor :markup t)
  (forget-yank editor)
  (let* ((chord (read-chord editor))
	 (command (gethash chord (editor-commands editor)
			   (if (characterp chord)
			       'add-char
			       'unknown-command))))
    (if (hash-table-p command)
        ;; prefix command
        (let* ((ch (read-chord editor))
               (com
                 (gethash chord command
                          (if (characterp chord)
                              'add-char
                              'unknown-command))))
          (setf *last-command* (cons chord com))
          (funcall com ch editor))
        ;; command
        (progn
          (funcall command chord editor)
          (setf *last-command* command))))
  (save-state editor))

(defun get-finished-string (editor)
  (buffer-push (get-string editor) (editor-history editor))
  (newline editor)
  (get-string editor))

(eval-always
  (defmacro with-editor-point-and-string (((point string) editor) &body forms)
    `(let ((,point (get-point ,editor))
	   (,string (get-string ,editor)))
       ,@forms)))

(defun editor-interrupt (editor)
  (without-backend editor
    (sb-posix:kill 0 sb-posix:sigint)))

(defun editor-stop (editor)
  (without-backend editor (sb-posix:kill 0 sb-posix:sigtstp)))

(defun editor-word-start (editor)
  "Returns the index of the first letter of current or previous word,
if the point is just after a word, or the point."
  (with-editor-point-and-string ((point string) editor)
    (if (or (not (at-delimiter-p string point))
	    (not (and (plusp point) (at-delimiter-p string (1- point)))))
	(1+ (or (position-if 'word-delimiter-p string :end point :from-end t)
		-1)) ; start of string
	point)))

(defun editor-previous-word-start (editor)
  "Returns the index of the first letter of current or previous word,
if the point was at the start of a word or between words."
  (with-editor-point-and-string ((point string) editor)
    (let ((tmp (cond ((at-delimiter-p string point)
		      (position-if-not 'word-delimiter-p string
				       :end point :from-end t))
		     ((and (plusp point) (at-delimiter-p string (1- point)))
		      (position-if-not 'word-delimiter-p string
				       :end (1- point) :from-end t))
		     (t point))))
      ;; tmp is always in the word whose start we want (or NIL)
      (1+ (or (position-if 'word-delimiter-p string
			   :end (or tmp 0) :from-end t)
	      -1)))))

(defun editor-word-end (editor)
  "Returns the index just beyond the current word or the point if
point is not inside a word."
  (with-editor-point-and-string ((point string) editor)
    (if (at-delimiter-p string point)
	point
	(or (position-if 'word-delimiter-p string :start point)
	    (length string)))))

(defun editor-next-word-end (editor)
  "Returns the index just beyond the last letter of current or next
word, if the point was between words."
  (with-editor-point-and-string ((point string) editor)
    (let ((tmp (if (at-delimiter-p string point)
		   (or (position-if-not 'word-delimiter-p string
					:start point)
		       (length string))
		   point)))
      ;; tmp is always in the word whose end we want (or already at the end)
      (or (position-if 'word-delimiter-p string :start tmp)
	  (length string)))))

(defun editor-word (editor)
  "Returns the current word the point is in or right after, or an
empty string."
  (let ((start (editor-word-start editor))
	(end (editor-word-end editor)))
    (subseq (get-string editor) start end)))

(defun editor-sexp-start (editor)
  (with-editor-point-and-string ((point string) editor)
    (setf point (loop for n from (min point (1- (length string))) downto 0
		      when (not (whitespacep (schar string n)))
		      return n))
    (case (and point (schar string point))
      ((#\) #\] #\}) (or (find-open-paren string point) 0))
      ((#\( #\[ #\{) (max (1- point) 0))
      (#\" (or (find-open-quote string point)
	       (max (1- point) 0)))
      (t (editor-previous-word-start editor)))))

(defun editor-sexp-end (editor)
  (with-editor-point-and-string ((point string) editor)
    (setf point (loop for n from point below (length string)
		      when (not (whitespacep (schar string n)))
		      return n))
    (case (and point (schar string point))
      ((#\( #\[ #\{) (or (find-close-paren string point)
			 (length string)))
      ((#\) #\] #\}) (min (1+ point) (length string)))
      (#\" (or (find-close-quote string (1+ point))
	       (min (1+ point) (length string))))
      (t (editor-next-word-end editor)))))

(defun editor-complete (editor)
  (funcall (editor-completer editor) (editor-word editor) editor))

(defun remember-yank (editor)
  (setf (editor-yank editor) (get-point editor)))

(defun forget-yank (editor)
  (shiftf (editor-last-yank editor) (editor-yank editor) nil))

(defun try-yank (editor)
  (setf (editor-yank editor) (editor-last-yank editor))
  (editor-yank editor))

(defun editor-replace-word (editor word)
  (with-editor-point-and-string ((point string) editor)
    (declare (ignore point))
    (let ((start (editor-word-start editor))
	  (end (editor-word-end editor)))
      (setf (get-string editor)
	    (concatenate 'simple-string (subseq string 0 start) word (subseq string end))
	    (get-point editor) (+ start (length word))))))

(defun in-quoted-string-p (editor)
  (quoted-p (get-string editor) (get-point editor)))

;;; Completion
(defun pathname-directory-pathname (pathname)
  (make-pathname :name nil :type nil
		 :defaults pathname))

(defun underlying-directory-p (pathname)
  (case (file-kind pathname)
    (:directory t)
    (:symbolic-link 
     (file-kind (merge-pathnames (sb-posix:readlink pathname) pathname)))))

(defun logical-pathname-p (pathname)
  (typep (pathname pathname) 'logical-pathname))

(defun check-logical-pathname (string)
  (acond 
   ((find (trim string :char-bag ":") (list-all-logical-host-names) :test 'string-equal)
    (list (replace string it)))
   ((ignore-errors 
     (namestring 
      (translate-logical-pathname 
       (unless (find #\; string)
         (concatenate 'string string ";")))))
    (list it))
   (t nil)))

;; simplified LPN completion (just shows the expansion if available)
(defun logical-pathname-complete (string)
  (when-let ((path (check-logical-pathname string)))
    (values path (length string))))

;;; We can't easily do zsh-style tab-completion of ~us into ~user, but
;;; at least we can expand ~ and ~user.  The other bug here at the
;;; moment is that ~nonexistant will complete to the same as ~.
(defun tilde-expand-string (string)
  "Returns the supplied string, with a prefix of ~ or ~user expanded
to the appropriate home directory."
  (if (and (> (length string) 0)
	   (eql (schar string 0) #\~))
      (flet ((chop (s) 
	       (subseq s 0 (1- (length s)))))
	(let* ((slash-index (loop for i below (length string)
				  when (eql (schar string i) #\/) 
				  return i))
	       (suffix (and slash-index (subseq string slash-index)))
	       (uname (subseq string 1 slash-index))
	       (homedir (or (cdr (assoc :home (std::user-info uname)))
			    (chop (namestring 
				   (or (probe-file (user-homedir-pathname))
				       (return-from tilde-expand-string 
					 string)))))))
	  (concatenate 'string homedir (or suffix ""))))
      string))

(defun directory-complete (string)
  (declare (simple-string string))
  (let* ((common nil)
	 (all nil)
	 (max 0)
	 (string (tilde-expand-string string))
	 (dir (pathname-directory-pathname string))
	 (namefun (if (relative-pathname-p string)
		      #'namestring
		      (lambda (x) (namestring (merge-pathnames x))))))
    (unless (and (underlying-directory-p dir)
		 (not (wild-pathname-p dir)))
      (return-from directory-complete (values nil 0)))
    (with-directory-iterator (%next dir)
      (loop for entry = (%next)
	    while entry
	    do (let* ((full (funcall namefun entry))
		      (diff (mismatch string full)))
                 (log:trace! "~& completed: ~A, diff: ~A~%" full diff)
		 (unless (and diff (< diff (length string)))
                   (log:trace! "~& common ~A mismatch ~A~&" common 
                               (mismatch common full))
		   (setf common (if common
				    (subseq common 0 (mismatch common full))
				    full)
			 max (max max (length full))
			 all (cons full all))))))
    (log:trace! "~&common: ~A~%" common)
    (if (or (null common)
	    (<= (length common) (length string)))
	(values all max)
	(values (list common) (length common)))))

(defun make-list-completer (completions)
  (lambda (str ed)
    (declare (ignore ed))
    (if (zerop (length str))
        (values completions (reduce 'max (mapcar 'length completions)))
        (when-let ((matches (flatten (mapcar (lambda (x) (when (uiop:string-prefix-p str x) x)) completions))))
          (values matches (reduce 'max (mapcar 'length matches)))))))

(defun lisp-complete (string editor)
  (declare (simple-string string))
  (when (plusp (length string))
    (if (in-quoted-string-p editor)
	(if (logical-pathname-p string)
	    (logical-pathname-complete string)
	    (directory-complete string))
	(let* ((length (length string))
	       (first-colon (position #\: string))
	       (last-colon (position #\: string :from-end t))
	       (state (and first-colon
			   (if (< first-colon last-colon)
			       :internal
			       :external)))
	       (package (and first-colon
			     (find-package (if (plusp first-colon)
					       (string-upcase
						(subseq string 0 first-colon))
					       :keyword))))
	       (hash (make-hash-table :test #'equal))
	       (common nil)
	       (max-len 0))
	  (labels ((stringify (symbol)
		     (if (upper-case-p (schar string 0))
			 (string symbol)
			 (string-downcase (string symbol))))
		   (push-name (name)
		     (setf common (if common
				      (subseq name 0 (mismatch common name))
				      name)
			   max-len (max max-len (length name))
			   (gethash name hash) name))
		   (select-symbol (symbol match)
		     (let ((name (stringify symbol))
			   (end (length match)))
		       (when (and (> (length name) end)	; Skip indetical
				  (equal match (subseq name 0 end)))
			 (push-name (concatenate 'simple-string string (subseq name end)))))))
	    ;; Skip empty strings
	    (when (plusp length)
	      (if package
		  ;; Symbols with explicit package prefixes.
		  (let* ((start (1+ last-colon))
			 (match (subseq string start)))
		    (ecase state
		      (:internal (std::do-internal-symbols (sym package)
				   (select-symbol sym match)))
		      (:external (do-external-symbols (sym package)
				   (select-symbol sym match)))))
		  ;; Symbols without explicit package prefix + packges
		  (dolist (package (list-all-packages))
		    (if (eq *package* package)
			(do-symbols (sym)
			  (select-symbol sym string))
			;; Package names
			(dolist (name (cons (package-name package)
					    (package-nicknames package)))
			  (select-symbol name string))))))
	    ;; Return list of matches to caller
	    (if (> (length common) (length string))
		(values (list common) (length common))
		(let (list)
		  (maphash (lambda (key val)
			     (declare (ignore val))
			     (push key list))
			   hash)
		  (values list max-len))))))))

;;; Main
(defun linedit (&rest keyword-args &key prompt history killring completions &allow-other-keys)
  "Reads a single line of input with line-editing from standard input
of the process and returns it as a string.

Results are unspecified if *STANDARD-INPUT* has been bound or altered.

PROMPT specifies the string to print to *STANDARD-OUTPUT* before
starting the accept input.

HISTORY and KILLRING can be pathname designators, in which case
they indicate the file to use for history and killring persistence,
respectively.

Further keyword arguments to LINEDIT are an advanced and undocumented
topic, but if you're willing to dive into sources you can eg. use
multiple kill-rings not shared between different invocations of
LINEDIT, or change the function responsible for providing input
completion."
  (declare (ignore prompt history killring))
  (flet ((edit ()
           (catch 'linedit-done
             (loop
	       (catch 'linedit-loop
		 (next-chord *editor*))))
           (redraw-line *editor*)
           (get-finished-string *editor*)))
    (if (and *editor* (backend-ready-p *editor*))
        ;; FIXME: This is a bit kludgy. It would be nicer to have a new
        ;; editor object that shares the same backend, kill-ring, etc.
        (let* ((new (getf keyword-args :prompt))
               (old (editor-prompt *editor*))
               (completer (editor-completer *editor*))
               (history (copy-buffer (editor-history *editor*)))
               (string (get-string *editor*))
               (point (get-point *editor*)))
          (unwind-protect
               (progn
                 (when new
                   (setf (editor-prompt *editor*) new))
                 (when completions
                   (setf (editor-completer *editor*) (make-list-completer completions)))
                 (edit))
            (when new
              (setf (editor-prompt *editor*) old))
            (when completions
              (setf (editor-completer *editor*) completer))
            (setf (get-string *editor*) string
                  (get-point *editor*) point
                  (editor-history *editor*) history)))
        (progn
          (setf *editor* (apply 'make-editor keyword-args))
          (with-backend *editor*
            (edit))))))

(defvar *line-level* 0)

(defun formedit (&rest args &key (prompt1 "") (prompt2 "") history killring completions
		 &allow-other-keys)
  "Reads a single form (s-expession) of input with line-editing from
standard input of the process and returns it as a string.

Results are unspecified if *STANDARD-INPUT* has been bound or altered,
or if *READTABLE* is not the standard readtable.

PROMPT1 specifies the string to print to *STANDARD-OUTPUT* before
starting the accept input.

PROMPT2 specifies the string to print to *STANDARD-OUTPUT* when input
spans multiple lines (ie. prefixing every but first line of input.)

HISTORY and KILLRING can be pathname designators, in which case
they indicate the file to use for history and killring persistence,
respectively.

Further keyword arguments to FORMEDIT are an advanced and undocumented
topic, but if you're willing to dive into sources you can eg. use
multiple kill-rings not shared between different invocations of
FORMEDIT, or change the function responsible for providing input
completion."
  (declare (ignore history killring completions))
  (let ((args (copy-list args)))
    (dolist (key '(:prompt1 :prompt2))
      (remf args key))
    (catch 'form-done
      (let ((eof-marker (gensym "EOF"))
	    (table (copy-readtable)))
	;; FIXME: It would be nice to provide an interace of some sort that
	;; the user could use to alter the crucial reader macros in custom readtables.
	(set-macro-character #\: #'colon-reader nil table)
	(set-macro-character #\, (constantly (values)) nil table)
	(set-macro-character #\; #'semicolon-reader nil table)
	(set-dispatch-macro-character #\# #\. (constantly (values)) table)
	(do ((str (apply #'linedit :prompt prompt1 args)
		  (concatenate 'simple-string str
			       (string #\newline)
			       (apply #'linedit :prompt prompt2 args))))
	    ((let ((form (handler-case (let ((*readtable* table)
                                             (*line-level* (1+ *line-level*))
					     (*package* (make-package
                                                         ;; If we manage to get into a nested read,
                                                         ;; make sure we don't try to use the same package.
                                                         (format nil "LINEDIT-SCRATCH#~A" *line-level*))))
					 (unwind-protect (read-from-string str)
					   (delete-package *package*)))
			   (end-of-file ()
			     eof-marker))))
	       (unless (eq eof-marker form)
		 (throw 'form-done str)))))))))

(defun semicolon-reader (stream char)
  (declare (ignore char))
  (loop for char = (read-char stream)
        until (eql char #\newline))
  (values))

(defun colon-reader (stream char)
  (declare (ignore char))
  (read stream t nil t))

;;; Command Functions
;; These functions are meant to be call throught the command table
;; of an editor. These functions should not explicitly call refresh, etc:
;; that is the responsibility of the editor -- but beeping is ok.

;; The arguments passed are: CHORD EDITOR

;;; BASIC EDITING
(defun add-char (char editor)
  (with-editor-point-and-string ((point string) editor)
    (setf (get-string editor)
          (concatenate 'simple-string (subseq string 0 point)
                       (string char)
                       (if (editor-insert-mode editor)
                           (subseq string point)
                           (when (> (length string) (1+ point))
                             (subseq string (1+ point))))))
    (incf (get-point editor))))

(defun delete-char-backwards (chord editor)
  (declare (ignore chord))
  (with-editor-point-and-string ((point string) editor)
    ;; Can't delegate to editor because of the SUBSEQ index calc.
    (unless (zerop point)
      (setf (get-string editor) (concatenate 'simple-string (subseq string 0 (1- point))
                                             (subseq string point))
            (get-point editor) (1- point)))))

(defun delete-char-forwards (chord editor)
  (declare (ignore chord))
  (with-editor-point-and-string ((point string) editor)
    (setf (get-string editor) (concatenate 'simple-string (subseq string 0 point)
                                           (subseq string (min (1+ point) (length string)))))))

(defun delete-char-forwards-or-eof (chord editor)
  (if (equal "" (get-string editor))
      (error 'end-of-file :stream *standard-input*)
      (delete-char-forwards chord editor)))

(defun delete-word-forwards (chord editor)
  (declare (ignore chord))
  (with-editor-point-and-string ((point string) editor)
    (declare (ignore point))
    (let ((i (get-point editor))
          (j (editor-next-word-end editor)))
      (setf (get-string editor)
            (concatenate 'simple-string (subseq string 0 i) (subseq string j))))))

(defun delete-word-backwards (chord editor)
  (declare (ignore chord))
  (with-editor-point-and-string ((point string) editor)
    (let ((i (editor-previous-word-start editor)))
      (setf (get-string editor) (concatenate 'simple-string (subseq string 0 i)
                                             (subseq string point))
            (get-point editor) i))))

(defun finish-input (chord editor)
  (declare (ignore chord editor))
  (throw 'linedit-done t))

;;; CASE CHANGES
(flet ((frob-case (frob editor)
         (with-editor-point-and-string ((point string) editor)
           (let ((end (editor-next-word-end editor)))
             (setf (get-string editor) (concatenate 'simple-string
                                                    (subseq string 0 point)
                                                    (funcall frob
                                                             (subseq string point end))
                                                    (subseq string end))
                   (get-point editor) end)))))

  (defun upcase-word (chord editor)
    (declare (ignore chord))
    (funcall #'frob-case #'string-upcase editor))

  (defun downcase-word (chord editor)
    (declare (ignore chord))
    (funcall #'frob-case #'string-downcase editor)))

;;; MOVEMENT
(defun move-to-bol (chord editor)
  (declare (ignore chord))
  (setf (get-point editor) 0))

(defun move-to-eol (chord editor)
  (declare (ignore chord))
  (setf (get-point editor) (length (get-string editor))))

(defun move-char-right (chord editor)
  (declare (ignore chord))
  (incf (get-point editor)))

(defun move-char-left (chord editor)
  (declare (ignore chord))
  (decf (get-point editor)))

(defun move-word-backwards (chord editor)
  (declare (ignore chord))
  (setf (get-point editor) (editor-previous-word-start editor)))

(defun move-word-forwards (chord editor)
  (declare (ignore chord))
  (setf (get-point editor) (editor-next-word-end editor)))

;;; UNDO
(defun undo (chord editor)
  (declare (ignore chord))
  (rewind-state editor)
  (throw 'linedit-loop t))

;;; HISTORY
(defun history-previous (chord editor)
  (declare (ignore chord))
  (let ((p (buffer-previous (get-string editor) (editor-history editor))))
    (if p
        (setf (get-string editor) p)
        (beep editor))))

(defun history-next (chord editor)
  (declare (ignore chord))
  (std:aif (buffer-next (get-string editor) (editor-history editor))
           (setf (get-string editor) std:it)
           (beep editor)))

(defvar *history-search* nil)
(defvar *history-needle* nil)

(defun history-search-needle (editor &key direction)
  (declare (ignore direction))
  (let ((text (if *history-search*
                  (cond ((and *history-needle*
                              (member *last-command* '(search-history-backwards
                                                       search-history-forwards)))
                         *history-needle*)
                        (t
                         (setf *history-needle* (get-string editor))))
                  (let* ((*history-search* t)
                         (*aux-prompt* nil))
                    (linedit :prompt "Search History: ")))))
    (when *history-search*
      (setf *aux-prompt* (concatenate 'simple-string "[" text "] ")))
    text))

(defun history-search (editor direction)
  (let* ((text (history-search-needle editor))
         (history (editor-history editor))
         (test (lambda (old) (search text old)))
         (match (unless (equal "" text)
                  (ecase direction
                    (:backwards
                     (buffer-find-previous-if test history))
                    (:forwards
                     (buffer-find-next-if test history))))))
    (unless match
      (beep editor)
      (setf match text))
    (setf (get-string editor) match
          (get-point editor) (length match))))

(defun search-history-backwards (chord editor)
  (declare (ignore chord))
  (history-search editor :backwards))

(defun search-history-forwards (chord editor)
  (declare (ignore chord))
  (history-search editor :forwards))

;;; KILLING & YANKING
(defun %yank (editor)
  (std:aif (buffer-peek (editor-killring editor))
           (with-editor-point-and-string ((point string) editor)
             (setf (get-string editor)
                   (concatenate 'simple-string (subseq string 0 (editor-yank editor))
                                std:it
                                (subseq string point))
                   (get-point editor) (+ (editor-yank editor) (length std:it))))
           (beep editor)))

(defun yank (chord editor)
  (declare (ignore chord))
  (remember-yank editor)
  (%yank editor))

(defun yank-cycle (chord editor)
  (declare (ignore chord))
  (if (try-yank editor)
      (progn
        (buffer-cycle (editor-killring editor))
        (%yank editor))
      (beep editor)))

(defun kill-to-eol (chord editor)
  (declare (ignore chord))
  (with-editor-point-and-string ((point string) editor)
    (buffer-push (subseq string point) (editor-killring editor))
    (setf (get-string editor) (subseq string 0 point))))

(defun kill-to-bol (chord editor)
  ;; Thanks to Andreas Fuchs
  (declare (ignore chord))
  (with-editor-point-and-string ((point string) editor)
    (buffer-push (subseq string 0 point) (editor-killring editor))
    (setf (get-string editor) (subseq string point)
          (get-point editor) 0)))

(defun copy-region (chord editor)
  (declare (ignore chord))
  (std:awhen (editor-mark editor)
    (with-editor-point-and-string ((point string) editor)
      (let ((start (min std:it point))
            (end (max std:it point)))
        (buffer-push (subseq string start end) (editor-killring editor))
        (setf (editor-mark editor) nil)))))

(defun cut-region (chord editor)
  (declare (ignore chord))
  (std:awhen (editor-mark editor)
    (with-editor-point-and-string ((point string) editor)
      (let ((start (min std:it point))
            (end (max std:it point)))
        (copy-region t editor)
        (setf (get-string editor) (concatenate 'simple-string (subseq string 0 start)
                                               (subseq string end))
              (get-point editor) start)))))

(defun set-mark (chord editor)
  (declare (ignore chord))
  ;; FIXME: this was (setf mark (unless mark point)) -- modulo correct
  ;; accessors.  Why? Was I not thinking, or am I not thinking now?
  (setf (editor-mark editor) (get-point editor)))

;;; SEXP MOTION
(defun forward-sexp (chord editor)
  (declare (ignore chord))
  (setf (get-point editor) (editor-sexp-end editor)))

(defun backward-sexp (chord editor)
  (declare (ignore chord))
  (setf (get-point editor) (editor-sexp-start editor)))

;; FIXME: KILL-SEXP is fairly broken, but works for enough of my
;; common use cases.  Most of its flaws lie in how the EDITOR-SEXP-
;; functions deal with objects other than lists and strings.
(defun kill-sexp (chord editor)
  (declare (ignore chord))
  (with-editor-point-and-string ((point string) editor)
    (declare (ignore point))
    (let ((start (editor-sexp-start editor))
          (end (min (1+ (editor-sexp-end editor)) (length string))))
      (buffer-push (subseq string start end) (editor-killring editor))
      (setf (get-string editor) (concatenate 'simple-string (subseq string 0 start)
                                             (subseq string end))
            (get-point editor) start))))

(defun close-all-sexp (chord editor)
  (move-to-eol chord editor)
  (do ((string (get-string editor) (get-string editor)))
      ((not (find-open-paren string (length string))))
    (add-char (case (schar string (find-open-paren string (length string)))
                (#\( #\))
                (#\[ #\])
                (#\{ #\}))
              editor)))

;;; SIGNALS
(defun interrupt-lisp (chord editor)
  (declare (ignore chord))
  (editor-interrupt editor))

(defun stop-lisp (chord editor)
  (declare (ignore chord))
  (editor-stop editor))

;;; MISCELLANY
(defun help (chord editor)
  (declare (ignore chord))
  (let ((pairs nil)
        (max-id 0)
        (max-f 0))
    (maphash (lambda (id function)
               (let ((f (string-downcase (symbol-name function))))
                 (push (list id f) pairs)
                 (setf max-id (max max-id (length id))
                       max-f (max max-f (length f)))))
             (editor-commands editor))
    (print-in-columns editor
                      (mapcar (lambda (pair)
                                (destructuring-bind (id f) pair
                                  (with-output-to-string (s)
                                    (write-string id s)
                                    (loop repeat (- (1+ max-id) (length id))
                                          do (write-char #\Space s))
                                    (write-string f s))))
                              (nreverse pairs))
                      :width (+ max-id max-f 2))))

(defun unknown-command (chord editor)
  (newline editor)
  (format *standard-output* "Unknown command ~S." chord)
  (newline editor))

(defun complete (chord editor)
  (declare (ignore chord))
  (multiple-value-bind (completions max-len) (editor-complete editor)
    (if completions
        (if (not (cdr completions))
            (editor-replace-word editor (car completions))
            (print-in-columns editor completions :width (+ max-len 2)))
        (beep editor))))

(defun apropos-word (chord editor)
  (declare (ignore chord))
  (let* ((word (editor-word editor))
         (apropi (apropos-list word)))
    (if (null apropi)
        (beep editor)
        (let* ((longest 0)
               (strings (mapcar (lambda (symbol)
                                  (declare (symbol symbol))
                                  (let ((str (prin1-to-string symbol)))
                                    (setf longest (max longest (length str)))
                                    (string-downcase str)))
                                apropi)))
          (print-in-columns editor strings :width (+ longest 2))))))

(defun describe-word (chord editor)
  (declare (ignore chord))
  (print-in-lines editor
                  (with-output-to-string (s)
                    (describe (read-from-string (editor-word editor)) s))))

(defun inspect-word (chord editor)
  (declare (ignore chord))
  (without-backend editor
    (inspect (read-from-string (editor-word editor)))))

(defun toggle-insert (chord editor)
  (declare (ignore chord))
  (setf (editor-insert-mode editor) (not (editor-insert-mode editor))))

(let (prompt-fun read-form-fun)
  (declare (type (or null function) prompt-fun read-form-fun))

  (macrolet ((enforce-consistent-state ()
	       `(assert (or (and prompt-fun read-form-fun)
			    (not (or prompt-fun read-form-fun))))))

    (defun uninstall-repl ()
      (enforce-consistent-state)
      (if prompt-fun
	  (setf sb-int:*repl-prompt-fun* prompt-fun
		sb-int:*repl-read-form-fun* read-form-fun
		prompt-fun nil
		read-form-fun nil)
	  (warn "UNINSTALL-REPL failed: No Linedit REPL present."))
      nil)

    (defun install-repl (&rest args &key wrap-current eof-quits history killring  &allow-other-keys)
      (enforce-consistent-state)
      (let ((args (copy-list args)))
	(dolist (key '(:wrap-current :eof-quits
		       :history :killring))
	  (remf args key))
	(when prompt-fun
	  (warn "INSTALL-REPL failed: Linedit REPL already installed.")
	  (return-from install-repl nil))
	(setf prompt-fun sb-int:*repl-prompt-fun*
	      read-form-fun sb-int:*repl-read-form-fun*)
	(flet ((repl-reader (in out)
		 (declare (type stream out)
			  (ignore in))
		 (fresh-line out)
		 (let ((prompt (with-output-to-string (s)
				 (funcall prompt-fun s))))
		   (handler-case
		       (apply #'formedit
			      :prompt1 prompt
			      :prompt2 (make-string (length prompt) 
						    :initial-element #\Space)
			      :history history
			      :killring killring
			      args)
		     (end-of-file (e)
                       (declare (ignore e))
		       (if eof-quits
			   (and (fresh-line) (eof-handler "SBCL" #'sb-ext:quit))
			   ;; Hackins, I know.
			   "#.''end-of-file"))))))
	  (setf sb-int:*repl-prompt-fun* (constantly ""))
	  (setf sb-int:*repl-read-form-fun*
		(if wrap-current
		    (lambda (in out)
		      (declare (type stream out in))
		      ;; FIXME: Yich.
		      (terpri)
		      (with-input-from-string (in (repl-reader in out))
			(funcall read-form-fun in out)))
		    (lambda (in out)
		      (declare (type stream out in))
		      (handler-case (read-from-string (repl-reader in out))
			(end-of-file ()
			  ;; We never get here if eof-quits is true, so...
			  (fresh-line)
			  (write-line "#<end-of-file>")
			  (values)))))))
	t))))
