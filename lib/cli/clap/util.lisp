;;; cli/clap/util.lisp --- Clap Utilities

;; 

;;; Code:
(in-package :cli/clap/util)

(defun arg0 () (car sb-ext:*posix-argv*))
(defun args () (cdr sb-ext:*posix-argv*))

(declaim (inline long-opt-p long-opt-has-eq-p
                 short-opt-p group-opt-p
                 opt-string-prefix-eq))

(defun long-opt-p (str)
  (declare (simple-string str))
  (and (> (length str) 2)
       (char= (aref str 0) (aref str 1) #\-)))

(defun short-opt-has-eq-p (str)
  "Return non-nil if STR is a short-opt which has an '=' somewhere,
indicating a key/val pair without whitespace."
  (declare (simple-string str))
  (when-let ((pos (position #\= str :test 'char=)))
    (cons (subseq str 1 pos) (subseq str (1+ pos)))))

(defun long-opt-has-eq-p (str)
  "Return non-nil if STR is a long-opt which has an '=' somewhere,
indicating a key/val pair without whitespace."
  (declare (simple-string str))
  (when-let ((pos (position #\= str :test 'char=)))
    (cons (subseq str 2 pos) (subseq str (1+ pos)))))

(defun short-opt-p (str)
  (declare (simple-string str))
  (and (char= (aref str 0) #\-)
       (= (length str) 2)
       (not (char= (aref str 1) #\-))))

(defun group-opt-p (str)
  (declare (simple-string str))
  (equalp str *cli-group-separator*))

(defun multi-short-opt-p (str)
  "Return non-nil if STR is a multi-short-opt - prefixed with a single '-' but
containing multiple characters."
  (declare (simple-string str))
  (and (char= (aref str 0) #\-)
       (not (char= (aref str 1) #\-))))

(defun opt-keyword-p (str)
  (declare (simple-string str))
  (char= (aref str 0) #\:))

(defun opt-string-prefix-eq (ch str)
  (char= ch (aref str 0)))

;; currently not in use
#+nil
(defun gen-thunk-ll (origin args)
  (let ((a0 (list (symbolicate '$a 0) origin)))
    (group 
     (nconc (loop for i from 1 for a in args nconc (list (symbolicate '$a (the fixnum i)) a)) a0)
     2)))

(defun default-cmd-thunk (args opts)
  (declare (ignore args opts))
  (values))

(defun default-opt-thunk (arg)
  (identity arg))

(defun cli-opt-type-p (s)
  (declare (type symbol s))
  (find s *cli-opt-types* :test 'string-equal))
