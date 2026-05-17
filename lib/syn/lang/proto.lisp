;;; proto.lisp --- SYN/LANG Protocol

;; 

;;; Code:
(in-package :syn/lang)

(defvar *lang* nil)

(defcondition lang-condition () ())

(defconfig lang (ast id)
  (flags
   tools
   features))

(defmacro deflang (name supers slots &rest options)
  `(defclass ,name ,(or supers '(lang)) ,slots ,@options))
