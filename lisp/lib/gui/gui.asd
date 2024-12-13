;;; gui.asd --- GUI subsystem
(defsystem :gui
  :depends-on (:std 
               :log :obj :xkb :parse
               (:feature :wl :wayflan) (:feature :wl :wayflan-client)
               (:feature :x11 :clx) (:feature :x11 :stumpwm)
               :cli :io)
  :components ((:file "pkg")
               (:file "err")
               (:file "server")
               (:file "client")
               (:module "wm"
                :components 
                ((:file "pkg")
                 #+wl 
                 (:module "wl"
                  :components 
                  ((:file "pkg")
                   (:file "kbd")
                   (:file "shell")))
                 #+x11
                 (:module "x11"
                  :components 
                  ((:file "pkg")
                   (:module "stump"
                    :components
                    ((:file "pkg")
                     (:file "var")
                     (:module "mod"
                      :components ((:file "disk")))))))))
               (:file "ext"))
  :in-order-to ((test-op (test-op "gui/tests"))))

(defsystem :gui/tests
  :depends-on (:rt :gui)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :gui)))
