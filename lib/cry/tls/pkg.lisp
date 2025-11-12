;;; pkg.lisp --- SSL

;; 

;;; Code:
(pkg:defpkg :cry/tls
  (:nicknames :tls :ssl)
  (:use :cl :std :sb-gray :io :config :build :sb-alien :cry-int)
  (:shadowing-import-from :std/rand :random-bytes)
  (:use-reexport :cl+ssl)
  (:export :*ssl-cipher-list* :*ssl-buffer-size* :*ca-bundle* :*no-ssl*))
(in-package :tls)
(defvar *ssl-cipher-list* nil)
(defvar *ssl-buffer-size* 2048)
(defvar *no-ssl* nil)
(defparameter *ca-bundle*
  #.(namestring #P"/etc/ca-certificates/extracted/ca-bundle.trust.crt")
  "The default public root certificates used for SSL verification.")
(define-condition ssl-condition (crypto-condition) ())
(define-condition ssl-error (error ssl-condition) ())
