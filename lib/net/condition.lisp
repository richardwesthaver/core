;;; net/condition.lisp --- Network Conditions

;; Generic network condition handling for Lisp.

;;; Code:
(in-package :net/core)

(define-condition net-condition () ())
(define-condition codec-condition (net-condition) ())
(define-condition protocol-condition (net-condition) ())

(define-condition net-error (net-condition std-error) ())
(define-condition codec-warning (codec-condition std-warning) ())
(define-condition protocol-warning (protocol-condition std-warning) ())
(define-condition codec-error (codec-condition net-error) ())
(define-condition protocol-error (protocol-condition net-error) ())
;; sb-bsd-sockets:socket-error
;; sb-thread:thread-error
