;;; uring.asd-*- mode: lisp; -*-
(defsystem :uring
  :depends-on (:std)
  :in-order-to ((test-op (test-op "uring/tests")))
  :components ((:file "uring"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :uring)))
