;;; net/srv.lisp --- Lisp Web Services

;; This library contains provides a Web Server abstraction a la Hunchentoot or
;; Woo.

;;; Commentary:

;; The code in this file is meant to be small. We want to leverage the core
;; ecosystem and internal NET/* packages to build high-level abstractions that
;; are still useful with minimal boilerplate.

;; In other words we want to support both these use-cases in the least amount
;; of code:
#|
(srv:start (srv:file-server)) ;; start a simple HTTP file server in current directory with all default values

(srv:define-web-service my-homepage :port 8080 :auth (auth settings ...) :routes (routes ...) &more ...)
(with-ws (ws 'my-homepage)
  (srv:start ws))
|#

;;;; NET/SANS-IO
;; This package contains the low-level base classes which are extended by this
;; library.

;;; Code:
(in-package :net/srv)

