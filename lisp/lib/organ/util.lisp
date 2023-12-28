;;; lib/organ/util.lisp --- Organ Utils

;;

;;; Code:
(in-package :organ)

(defun peek-line (stream)
  (concatenate 
   'string 
   (loop for c = (peek-char nil stream nil :eof)
         until (char= c #\newline)
         collect c)))

(defun read-until-end (stream)
  (with-output-to-string (s)
    (loop for c = (read-char stream nil :eof)
          until (eql c :eof)
          do (write-char c s))))

(defun read-org-lines (&optional stream)
  (apply #'vector
	 (loop for l = (read-line stream nil :eof)
	       until (eq l :eof)
	       collect l)))

(defun read-org-lines-from-string (str)
  (with-input-from-string (s str) (read-org-lines s)))
