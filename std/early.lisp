;;; early.lisp --- A few early STD macros

;; 

;;; Code:
(in-package :std/early)

;;; EARLY MACROS
(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
		"G!"
		:start1 0
		:end1 2)))

(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
		"O!"
		:start1 0
		:end1 2)))

(defun o!-symbol-to-g!-symbol (s)
  (symb "G!"
	(subseq (symbol-name s) 2)))

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
	       (remove-if-not #'g!-symbol-p
			      (flatten body)))))
    (multiple-value-bind (body declarations docstring)
	(parse-body body :documentation t)
      `(defmacro ,name ,args
	 ,@(when docstring
	     (list docstring))
	 ,@declarations
	 (let ,(mapcar
		(lambda (s)
		  `(,s (gensym ,(subseq
				 (symbol-name s)
				 2))))
		syms)
	   ,@body)))))

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p (flatten args)))
	 (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    (multiple-value-bind (body declarations docstring)
	(parse-body body :documentation t)
      `(defmacro/g! ,name ,args
	 ,@(when docstring
	     (list docstring))
	 ,@declarations
	 `(let ,(mapcar #'list (list ,@gs) (list ,@os))
	    ,(progn ,@body))))))

(defmacro defun! (name args &body body)
  (let ((syms (remove-duplicates
	       (remove-if-not #'g!-symbol-p
			      (flatten body)))))
    (multiple-value-bind (body declarations docstring)
	(parse-body body :documentation t)
      `(defun ,name ,args
	 ,@(when docstring
	     (list docstring))
	 ,@declarations
	 (let ,(mapcar (lambda (s)
			 `(,s (gensym ,(subseq (symbol-name s)
					       2))))
		syms)
	   ,@body)))))
