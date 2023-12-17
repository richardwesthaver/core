(in-package :net/core)

(define-condition net-error (error) ())

(define-condition codec-error (net-error) ())
(define-condition protocol-error (net-error) ())

;; sb-bsd-sockets:socket-error
;; sb-thread:thread-error
