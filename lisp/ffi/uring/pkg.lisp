;;; uring/pkg.lisp --- URING Systems

;; /usr/include/liburing.h

;;; Commentary:

;; IO_URING is our preferred means of IO on Linux. The bindings here
;; are used by the high-level library IO.

;; As a point of reference, we look to the SB-SYS:SERVE-EVENT function
;; in SBCL. This is an async event loop which dispatches to a backend
;; based on features. On Linux it will use either poll or select(2),
;; neither of which are particularly fast.

;; Using the bindings provided by this library we will implement an
;; alternative backend to dispatch to.

;;; Code:
(defpackage :uring
  (:use :cl :std :sb-alien)
  (:export :load-uring))

(in-package :uring)
(define-alien-loader "uring" t "/usr/lib/")
;;; io_uring_version.h
(define-alien-routine io-uring-major-version int)
(define-alien-routine io-uring-minor-version int)
(define-alien-routine io-uring-check-version int (int int))

;;; barrier.h
;; (defun io-uring-write-once (var val))
;; (defun io-uring-read-once (var))
;; (defun io-uring-smp-store-release (p v))
;; (defun io-uring-smp-load-acquire (p))
;; (defun io-uring-smp-mb ())

;;; io_uring.h

