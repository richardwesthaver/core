;;; lib/organ/util.lisp --- Organ Utils

;;

;;; Code:
(in-package :organ)

(defun read-org-lines (&optional stream)
  (apply #'vector
	 (loop for l = (read-line stream nil :eof)
	       until (eq l :eof)
	       collect l)))

(defun read-org-lines-from-string (str)
  (with-input-from-string (s str) (read-org-lines s)))
