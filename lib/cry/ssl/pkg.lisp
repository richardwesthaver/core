;;; pkg.lisp --- SSL

;; 

;;; Code:
(pkg:defpkg :cry/ssl
  (:nicknames :ssl)
  (:use :cl :std :sb-gray :io :config :build :sb-alien :rustls)
  (:shadowing-import-from :std/rand :random-bytes)
  (:use-reexport :cl+ssl)
  (:export :*ssl-cipher-list* :*ssl-buffer-size*))

(defvar *ssl-cipher-list* nil)
(defvar *ssl-buffer-size* 2048)
