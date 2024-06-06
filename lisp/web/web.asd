(defsystem :web/index
  :depends-on (:uiop :cl-ppcre :std :rdb :hunchentoot :parenscript :lass :spinneret :organ)
  :components ((:file "index"))
  :in-order-to ((test-op (test-op "app/tests")))
  :build-operation "program-op"
  :build-pathname "web-index"
  :entry-point "web/index::main")

(defsystem :web/dash
  :depends-on (:uiop :cl-ppcre :std :rdb :parenscript :lass :spinneret :organ)
  :components ((:file "dash"))
  :in-order-to ((test-op (test-op "app/tests")))
  :build-operation "program-op"
  :build-pathname "web-dash"
  :entry-point "web/dash::main")

(defsystem :web
  :depends-on (:web/dash :web/index))
