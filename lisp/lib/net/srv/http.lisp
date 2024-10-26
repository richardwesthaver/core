;;; http.lisp --- HTTP Services

;; HTTP/S Services (based on Hunchentoot)

;;; Commentary:

;; This module contains the main HTTP/S web application server machinery for
;; core modules. Loading this file should give you the basics needed to build
;; a CLOS-based asynchronous web server.

;;; Code:
(in-package :net/srv/http)
