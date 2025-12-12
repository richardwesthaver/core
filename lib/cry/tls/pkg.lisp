;;; pkg.lisp --- SSL

;; 

;;; Code:
(pkg:defpkg :cry/tls
  (:nicknames :tls :ssl)
  (:use :cl :std :sb-gray :io :config :build :sb-alien :cry-int :openssl)
  (:shadowing-import-from :std/rand :random-bytes)
  (:shadowing-import-from :cl+ssl :ssl-ctx-free)
  (:use-reexport :cl+ssl)
  (:export :*ssl-cipher-list* :*ssl-buffer-size* :*ca-bundle* :*no-ssl*))

(in-package :tls)
