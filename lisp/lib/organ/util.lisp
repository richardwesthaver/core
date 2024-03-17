;;; lib/organ/util.lisp --- Organ Utils

;;

;;; Code:
(in-package :organ)

(defun peek-line (stream)
  (concatenate 
   'string 
   (loop for c = (peek-char nil stream nil)
         until (or (not c) (char= c #\newline))
         collect c)))

(defun read-until-end (stream)
  (with-output-to-string (s)
    (loop for c = (read-char stream nil)
          until (not c)
          do (write-char c s))))

(defun read-org-lines (&optional stream)
  (apply #'vector
	 (loop for l = (read-line stream nil)
	       until (not l)
	       collect l)))

(defun read-org-lines-from-string (str)
  (with-input-from-string (s str) (read-org-lines s)))
