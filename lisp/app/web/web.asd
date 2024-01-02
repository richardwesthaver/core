#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem :web
  :depends-on (:web/dash :web/index)
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :rt :do-tests :app)))

(defsystem :web/index
  :depends-on (:uiop :cl-ppcre :std :rdb :hunchentoot :parenscript :lass :spinneret :organ)
  :components ((:file "index"))
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
