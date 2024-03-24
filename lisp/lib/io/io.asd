(defsystem :io
  :description "Data formats"
  :depends-on (:cl-ppcre :std :obj :uring)
  :version "0.1.0"
  :serial t
  :components ((:file "pkg"))
  :in-order-to ((test-op (test-op "io/tests"))))

(defsystem :io/tests
  :depends-on (:rt :io)
  :perform (test-op (o c) (symbol-call :rt :do-tests :io)))
