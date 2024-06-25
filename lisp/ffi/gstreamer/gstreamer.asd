;;; gstreamer.asd --- Gstreamer Sytem Definitions
(defsystem :gstreamer
  :depends-on (:std :log :glib :sb-grovel)
  :components ((:file "pkg")
               (:file "clock" :depends-on ("pkg"))
               (:file "object" :depends-on ("pkg"))
               (:file "bus" :depends-on ("object"))
               (:file "element" :depends-on ("clock" "object"))
               (:file "bin" :depends-on ("element"))
               (:file "debug" :depends-on ("bin")))
  :in-order-to ((test-op (test-op "gstreamer/tests"))))

(defsystem :gstreamer/tests
  :depends-on (:std :log :rt :gstreamer :glib)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :gstreamer)))
