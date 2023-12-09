;;; xkb.asd-*- mode: lisp; -*-
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage :xkb.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :xkb.sys)

(defsystem :xkb
  :depends-on (:sb-grovel :std)
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :xkb))
  :in-order-to ((test-op (test-op :xkb/tests))))

(defsystem :xkb/tests
  :depends-on (:std/rt :xkb)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :xkb)))
