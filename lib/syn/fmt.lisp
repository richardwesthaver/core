;;; fmt.lisp --- Syntax Formatting

;; 

;;; Commentary:

;; This module provides a simple interface for customizing the
;; formatting/style of Programming Languages. The files you produce with this
;; module are intended to be consumed by programs which will make sense of the
;; 'rules' you define - the most important use-case we have in mind is of
;; course Emacs, where SLIME (slime-cl-indent.el) and other Lisp-specific
;; packages provide a rich framework for consuming user-config in this
;; manner. This module is also available for use throughout the core - such as
;; the SYN/LINT module.

;;; Code:
(in-package :syn/fmt)

(defconfig fmt (ast) 
  ((name :accessor name)
   inherit
   indentation
   syntax
   theme))

(defmethod make-config ((self (eql :fmt)) &rest args)
  (apply 'make-instance 'fmt args))

(definline make-fmt (&rest args)
  (apply 'make-instance 'fmt args))

(defun write-fmt (fmt output)
  (declare (fmt fmt))
  (write-ast (build fmt) output))

(defun read-fmt (input)
  (let ((obj (read-ast (make-instance 'fmt) input)))
    (unless (slot-boundp obj 'name)
      (setf (name obj) (pathname-name input)))))
