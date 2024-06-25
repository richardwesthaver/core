;;; gstreamer.asd --- Gstreamer Sytem Definitions
(defsystem :gstreamer
  :depends-on (:std :log :glib :sb-grovel)
  :components ((:file "pkg")
               (:file "object" :depends-on ("pkg"))
               (:file "clock" :depends-on ("object"))
               (:file "bus" :depends-on ("object"))
               (:file "pad" :depends-on ("object"))
               (:file "element" :depends-on ("clock" "object"))
               (:file "bin" :depends-on ("element"))
               (:file "debug" :depends-on ("bin"))
               (:file "play" :depends-on ("element")))
  :in-order-to ((test-op (test-op "gstreamer/tests"))))

(defsystem :gstreamer/tests
  :depends-on (:std :log :rt :gstreamer :glib)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :gstreamer)))
