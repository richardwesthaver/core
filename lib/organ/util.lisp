;;; lib/organ/util.lisp --- Organ Utils

;;

;;; Code:
(in-package :organ)

(defun read-org-lines (&optional stream)
  (apply #'vector
	 (loop for l = (read-line stream nil)
	       until (not l)
	       collect l)))

(defun read-org-lines-from-string (str)
  (with-input-from-string (s str) (read-org-lines s)))

;; (sym-to-org-class-name 'headline)
(eval-always
(defun sym-to-org-class-name (sym) 
  "Convert keyword or symbol SYM to a symbol which could designate an ORG- object type."
  (intern (format nil "~:@(~a~a~)" "org-" sym) :organ)))
