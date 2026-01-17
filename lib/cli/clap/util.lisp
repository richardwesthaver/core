;;; cli/clap/util.lisp --- Clap Utilities

;; 

;;; Code:
(in-package :cli/clap)

(defun arg0 () (car sb-ext:*posix-argv*))
(defun args () (cdr sb-ext:*posix-argv*))

(definline long-opt-p (str)
  (declare (simple-string str))
  (and (> (length str) 2)
       (char= (aref str 0) (aref str 1) #\-)))

(definline short-opt-has-eq-p (str)
  "Return non-nil if STR is a short-opt which has an '=' somewhere,
indicating a key/val pair without whitespace."
  (declare (simple-string str))
  (when-let ((pos (position #\= str :test 'char=)))
    (cons (subseq str 1 pos) (subseq str (1+ pos)))))

(definline long-opt-has-eq-p (str)
  "Return non-nil if STR is a long-opt which has an '=' somewhere,
indicating a key/val pair without whitespace."
  (declare (simple-string str))
  (when-let ((pos (position #\= str :test 'char=)))
    (cons (subseq str 2 pos) (subseq str (1+ pos)))))

(definline short-opt-p (str)
  (declare (simple-string str))
  (and (char= (aref str 0) #\-)
       (= (length str) 2)
       (not (char= (aref str 1) #\-))))

(definline group-opt-p (str)
  (declare (simple-string str))
  (equalp str *cli-group-separator*))

(definline multi-short-opt-p (str)
  "Return non-nil if STR is a multi-short-opt - prefixed with a single '-' but
containing multiple characters."
  (declare (simple-string str))
  (and (char= (aref str 0) #\-)
       (not (char= (aref str 1) #\-))))

(definline opt-keyword-p (str)
  (declare (simple-string str))
  (char= (aref str 0) #\:))

(definline opt-string-prefix-eq (ch str)
  (char= ch (aref str 0)))

(defun schar0 (name)
  "Return the first char of symbol or string NAME."
  (schar (string name) 0))

(defmacro with-cli-handlers (&body body)
  "A wrapper which handles common cli errors that may occur during
evaluation of BODY."
  `(progn
     (if *no-debug*
         (sb-ext:disable-debugger)
         (sb-ext:enable-debugger))
     (unwind-protect
          (restart-case 
              (handler-case (progn ,@body)
                (sb-sys:interactive-interrupt (c)
                  (if *no-debug*
                      (sb-ext:exit :code 130)
                      c))
                (error (c)
                  (println c)
                  (sb-ext:exit :code 1)))
            (abort ()
              :report (lambda (s)
                        (write-string
                         "Skip to toplevel READ/EVAL/PRINT loop."
                         s)
                        (log:debug! "CONTINUEing from pre-REPL RESTART-CASE")
                        (values)))
            (exit ()
              :report "Exit SBCL (calling #'EXIT, killing the process)."
              ;; :test (lambda (c) (declare (ignore c)) t)
              (log:debug! "falling through to EXIT from pre-REPL RESTART-CASE~&")
              (exit :code 1))))
     (sb-impl::flush-standard-output-streams)
     (unless *no-exit*
       (exit :code 0))
     ;; reset terminal state
     #+nil (.ris)))
