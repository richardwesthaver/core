;;; pkg.lisp --- SSL

;; 

;;; Code:
(pkg:defpkg :cry/ssl
  (:nicknames :ssl)
  (:use :cl :std :sb-gray :io :config :build :sb-alien :rustls :cry)
  (:shadowing-import-from :std/rand :random-bytes)
  (:use-reexport :cl+ssl)
  (:export :*ssl-cipher-list* :*ssl-buffer-size*))

(in-package :ssl)
(defvar *ssl-cipher-list* nil)
(defvar *ssl-buffer-size* 2048)

(define-condition ssl-condition (crypto-condition) ())
(define-condition ssl-error (error ssl-condition) ())
