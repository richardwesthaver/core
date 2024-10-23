;;; pkg.lisp --- Lisp Code Generators

;; Lisp Codegen including (Portable) Common Lisp, Emacs Lisp, and Scheme.

;;; Code:
(defpackage :syn/gen/lisp
  (:use :cl :syn/gen))

(defpackage :syn/gen/lisp/cl
  (:nicknames :gencl)
  (:use :cl :syn/gen/lisp))

(defpackage :syn/gen/lisp/el
  (:nicknames :genel :el)
  (:use :cl :syn/gen/lisp))

(defpackage :syn/gen/lisp/scm
  (:nicknames :genscm :scm)
  (:use :cl :syn/gen/lisp))
