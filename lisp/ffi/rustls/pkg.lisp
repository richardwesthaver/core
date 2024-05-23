;;; pkg.lisp --- low-level bindings to librustls

;;; Commentary:

;;; Code:
(defpackage :rustls
  (:use :cl :sb-alien :std/alien)
  (:export :load-rustls))

(in-package :rustls)

(define-alien-loader "rustls" t "/usr/lib/")

(define-alien-type rustls-result unsigned-int)

(define-alien-type rustls-tls-version (enum nil (rustls-tls-version-sslv2 512)))

(define-alien-type rustls-accepted (struct rustls-accepted))

(define-alien-type rustls-accepted-alert (struct rustls-accepted-alert))
