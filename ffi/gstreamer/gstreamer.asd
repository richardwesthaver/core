;;; gstreamer.asd --- Gstreamer Sytem Definitions
(defsystem :gstreamer
  :depends-on (:std :log :glib)
  :components ((:file "pkg")
               (:file "object" :depends-on ("pkg"))
               (:file "clock" :depends-on ("object"))
               (:file "mini-object" :depends-on ("pkg"))
               (:file "caps" :depends-on ("mini-object"))
               (:file "context" :depends-on ("pkg"))
               (:file "task" :depends-on ("object"))
               (:file "iterator" :depends-on ("pkg"))
               (:file "bus" :depends-on ("object"))
               (:file "pad" :depends-on ("object"))
               (:file "element" :depends-on ("clock" "object" "iterator" "context"))
               (:file "element-factory" :depends-on ("element"))
               (:file "bin" :depends-on ("element"))
               (:file "debug" :depends-on ("bin"))
               (:file "play" :depends-on ("element")))
  :in-order-to ((test-op (test-op "gstreamer/tests"))))

(defsystem :gstreamer/tests
  :depends-on (:std :log :rt :gstreamer :glib)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :gstreamer)))
