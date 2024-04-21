;;; krypt/pkg.lisp --- Krypt Packages

;;

;;; Code:
(defpackage :krypt
  (:use :cl :std :cry :dat/sxp :obj/id)
  (:export :krypt-error :*default-user-kryptrc*
           :krypt-config :load-kryptrc))
