(defsystem :web/index
  :defsystem-depends-on (:lass)
  :depends-on (:uiop :cl-ppcre :std :rdb :hunchentoot :parenscript :lass :spinneret :organ)
  :components ((:file "index")
               (:lass-file "style"))
  :in-order-to ((test-op (test-op "app/tests")))
  :build-operation "program-op"
  :build-pathname "web-index"
  :entry-point "app/web/index::main")

(defsystem :web/dash
  :depends-on (:uiop :cl-ppcre :std :rdb :lack :clack :parenscript :lass :spinneret :organ)
  :components ((:file "dash"))
  :in-order-to ((test-op (test-op "app/tests")))
  :build-operation "program-op"
  :build-pathname "web-dash"
  :entry-point "app/web/dash::main")
