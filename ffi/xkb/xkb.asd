;;; xkb.asd-*- mode: lisp; -*-
(defsystem :xkb
  :depends-on (:std)
  :components ((:file "pkg")
               (sb-grovel:grovel-constants-file "constants"
                                      :package :xkb))
  :in-order-to ((test-op (test-op :xkb/tests))))

(defsystem :xkb/tests
  :depends-on (:rt :xkb)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :xkb)))
