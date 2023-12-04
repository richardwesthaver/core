;;; app.asd --- application library
(defsystem :app
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on 
  (:uiop :cl-ppcre :std/all :skel :organ :rdb 
   :cli/skel :app/cli/organ :app/cli/homer
   :app/gui/skel
   :app/web/index :app/web/dash)
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :std/rt :do-tests :app)))

(defsystem :app/web/index
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system :lass)
  :depends-on (:uiop :cl-ppcre :std/all :rdb :hunchentoot :parenscript :lass :spinneret :organ)
  :components ((:lass-file "web/style"))
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :std/rt :do-tests :app))
  :build-operation "program-op"
  :build-pathname "web-index"
  :entry-point "app/web/index::main")

(defsystem :app/web/dash
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:uiop :cl-ppcre :std/all :rdb :lack :clack :parenscript :lass :spinneret :organ)
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :std/rt :do-tests :app))
  :build-operation "program-op"
  :build-pathname "web-dash"
  :entry-point "app/web/dash::main")
