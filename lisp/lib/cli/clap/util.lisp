;;; cli/clap/util.lisp --- Clap Utilities

;; 

;;; Code:
(in-package :cli/clap/util)

(defun arg0 () (car sb-ext:*posix-argv*))
(defun args () (cdr sb-ext:*posix-argv*))

(defun long-opt-p (str)
  (declare (simple-string str))
  (and (char= (aref str 0) (aref str 1) #\-)
       (> (length str) 2)))

(defun short-opt-p (str)
  (declare (simple-string str))
  (and (char= (aref str 0) #\-)
       (not (char= (aref str 1) #\-))
       (> (length str) 1)))

(defun opt-group-p (str)
  (declare (simple-string str))
  (equalp str *cli-group-separator*))

(defun opt-string-prefix-eq (ch str)
  (declare (simple-string str) (character ch))
  (char= ch (aref str 0)))

;; currently not in use
(defun gen-thunk-ll (origin args)
  (let ((a0 (list (symbolicate '$a 0) origin)))
    (group 
     (nconc (loop for i from 1 for a in args nconc (list (symbolicate '$a (the fixnum i)) a)) a0)
     2)))

(defun default-thunk (args opts)
  (declare (ignore args opts))
  (values))

(defun cli-opt-kind-p (s)
  (declare (type symbol s))
  (find s *cli-opt-kinds* :test 'string-equal))
