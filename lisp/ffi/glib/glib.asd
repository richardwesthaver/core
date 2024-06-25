;;; glib.asd --- Glib Sytem Definitions
(defsystem :glib
  :depends-on (:std :log)
  :description "A thin FFI wrapper for Glib, used to support dynamically-loaded foreign
libraries which expose parts of their API via GObjects."
  :components ((:file "pkg"))
  :in-order-to ((test-op (test-op "glib/tests"))))

(defsystem :glib/tests
  :depends-on (:std :log :rt :glib)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :glib)))
