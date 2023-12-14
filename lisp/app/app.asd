;;; app.asd --- application library
(defsystem :app
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on 
  (:uiop :cl-ppcre :std :skel :organ :rdb :cli :log
   :bin/skel :bin/organ :bin/homer
   :gui/skel
   :web/index :web/dash)
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :rt :do-tests :app)))
