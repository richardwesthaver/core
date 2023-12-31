(defpackage :uring
  (:use :cl :std))

(in-package :uring)

(define-alien-loader "uring" t)
