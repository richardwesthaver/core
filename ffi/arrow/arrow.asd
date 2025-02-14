;;; arrow.asd --- Apache Arrow Sytem Definitions
(defsystem :arrow
  :depends-on (:std :log)
  :description "A thin FFI wrapper for Arrow."
  :components ((:file "pkg"))
  :in-order-to ((test-op (test-op "arrow/tests"))))

(defsystem :arrow/tests
  :depends-on (:std :log :rt :arrow)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :arrow)))
