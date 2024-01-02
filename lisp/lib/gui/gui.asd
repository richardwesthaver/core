;;; gui.asd --- GUI subsystem
(defsystem :gui
  :depends-on (:std :log :obj :wayflan :wayflan-client)
  :components ((:file "pkg")
               (:file "err")
               (:file "wm")
               (:module "proto"
                :components ((:file "keyboard")))
               (:file "ext"))
  :in-order-to ((test-op (test-op "gui/tests"))))

(defsystem :gui/tests
  :depends-on (:rt :gui)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :gui)))
