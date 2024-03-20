;;; uring/pkg.lisp --- URING Systems

;; /usr/include/liburing.h

;;; Code:
(defpackage :uring
  (:use :cl :std :sb-alien)
  (:export :load-uring))

(in-package :uring)
(define-alien-loader "uring" t "/usr/lib/")

(define-alien-routine io-uring-major-version int)
(define-alien-routine io-uring-minor-version int)
(define-alien-routine io-uring-check-version int (int int))
