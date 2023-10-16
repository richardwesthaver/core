;;; uring.asd-*- mode: lisp; -*-
(defsystem :uring
  :depends-on (:macs)
  :in-order-to ((test-op (test-op "uring/tests")))
  :components ((:file "uring/uring")))

(defsystem :uring/tests
  :depends-on (:uring :rt)
  :components ((:file "uring/tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :uring)))
