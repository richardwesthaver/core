;;; uring.asd-*- mode: lisp; -*-
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage :uring.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :uring.sys)

(defsystem :uring
  :depends-on (:sb-grovel :std :obj)
  :serial t
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :uring)
               (:file "util")
               (:file "macs")
               (:file "alien")
               (:file "prim")
               (:file "opcode")
               (:file "register")
               (:file "submit")
               (:file "sq")
               (:file "cq")
               (:file "uring"))
  :in-order-to ((test-op (test-op :uring/tests))))

(defsystem :uring/tests
  :depends-on (:rt :uring :obj)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :uring)))
