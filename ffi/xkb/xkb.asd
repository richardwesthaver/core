;;; xkb.asd-*- mode: lisp; -*-
(defpackage :xkb.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :xkb.sys)

(defsystem :xkb
  :depends-on (:std)
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :xkb))
  :in-order-to ((test-op (test-op :xkb/tests))))

(defsystem :xkb/tests
  :depends-on (:rt :xkb)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :xkb)))
