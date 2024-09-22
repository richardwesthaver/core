;;; skel/net/pkg.lisp --- Skel Networking

;; 

;;; Code:
(defpackage :skel/net/client
  (:use :cl :std :net :skel/core/proto :skel/core/obj))

(defpackage :skel/net/server
  (:use :cl :std :net :skel/core/proto :skel/core/obj))
