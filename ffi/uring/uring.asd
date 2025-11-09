;;; uring.asd-*- mode: lisp; -*-
;; (require 'sb-grovel)
(defpackage :uring.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :uring.sys)

(defsystem :uring
  :depends-on (:std :obj)
  :serial t
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :uring)
               (:file "util")
               (:file "macs")
               (:file "alien")
               (:file "opcode")
               (:file "prim")
               (:file "register")
               (:file "submit")
               (:file "sq")
               (:file "cq")
               (:file "uring"))
  :in-order-to ((test-op (test-op :uring/tests))))

(defsystem :uring/tests
  :depends-on (:rt :uring)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :uring)))
