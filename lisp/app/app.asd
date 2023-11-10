;;; app.asd --- application library
(defsystem :app
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:uiop :cl-ppcre :std/all :skel :organ :rdb :app/cli :app/gui :app/web)
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :std/rt :do-tests :app)))
