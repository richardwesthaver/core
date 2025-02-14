;;; pkg.lisp --- SSL

;; 

;;; Code:
(push :ssl *features*)
(pkg:defpkg :cry/ssl
  (:nicknames :ssl)
  (:use :cl :std :sb-gray :io :config :build :sb-alien :rustls)
  (:use-reexport :cl+ssl))
