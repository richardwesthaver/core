;;; pratt.lisp --- Simple Pratt Top Down Operator Precedence Parser

;; ref: https://tdop.github.io

;;; Code:
(in-package :parse/pratt)

(defclass pratt-parser () ())

(defgeneric next-precedence (self))

(defgeneric parse-prefix (self))

(defgeneric parse-infix (self left precedence))

(defmethod parse ((self pratt-parser) &optional (precedence 0))
  (let ((expr (parse-prefix self)))
    (loop while (< precedence (next-precedence self))
          do (setf expr (parse-infix self expr (next-precedence self))))
    expr))
