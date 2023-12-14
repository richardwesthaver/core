;;; uring.asd-*- mode: lisp; -*-
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage :uring.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :uring.sys)

(defsystem :uring
  :depends-on (:sb-grovel :std)
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :uring))
  :in-order-to ((test-op (test-op :uring/tests))))

(defsystem :uring/tests
  :depends-on (:rt :uring)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :uring)))
