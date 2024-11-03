(defsystem :web
  :depends-on (:std :net :obj :dat :parse :doc :organ :syn :cry :nlp :wasmer)
  :components ((:file "pkg")
               (:module "wasm"
                :components
                ((:file "pkg")
                 (:file "text")
                 (:file "binary")
                 (:file "rt"))))
  :in-order-to ((test-op (test-op "web/tests"))))

(defsystem :web/tests
  :depends-on (:rt :net :web)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :web)))

(defsystem :web/index
  :depends-on (:uiop :cl-ppcre :std :rdb :hunchentoot :parenscript :lass :spinneret :organ)
  :components ((:file "index"))
  :in-order-to ((test-op (test-op "web/tests")))
  :build-operation "program-op"
  :build-pathname "web-index"
  :entry-point "web/index::main")

(defsystem :web/dash
  :depends-on (:uiop :cl-ppcre :std :rdb :parenscript :lass :spinneret :organ)
  :components ((:file "dash"))
  :in-order-to ((test-op (test-op "web/tests")))
  :build-operation "program-op"
  :build-pathname "web-dash"
  :entry-point "web/dash::main")
