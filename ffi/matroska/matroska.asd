;;; matroska.asd --- Matroska Sytem Definitions
(defsystem :matroska
  :depends-on (:std :log)
  :description "A thin FFI wrapper for libmatroska (mkv/mka/etc)."
  :components ((:file "pkg"))
  :in-order-to ((test-op (test-op "matroska/tests"))))

(defsystem :matroska/tests
  :depends-on (:std :log :rt :matroska)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :matroska)))
