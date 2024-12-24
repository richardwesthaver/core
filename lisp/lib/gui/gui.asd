;;; gui.asd --- GUI subsystem
(defsystem :gui
  :depends-on (:std 
               :log :obj :xkb :parse
               (:feature :wl :wayflan) (:feature :wl :wayflan-client)
               (:feature :x11 :clx) 
               (:feature :x11 :stumpwm)
               (:feature :clim :mcclim)
               (:feature (:and :clim :dbg) :clim-debugger)
               :cli :io)
  :components ((:file "pkg")
               (:file "condition")
               (:file "server")
               (:file "client")
               (:module "clim"
                :if-feature :clim
                :components
                ((:file "pkg")
                 (:file "dbg" :if-feature :dbg)))
               (:module "wl"
                :if-feature :wl
                :components 
                ((:file "pkg")
                 (:file "kbd")
                 (:file "shell")))
               (:module "x11"
                :if-feature :x11
                :components 
                ((:file "pkg")
                 (:module "stump"
                  :components
                  ((:file "pkg")
                   (:file "var")
                   (:module "mod"
                    :components ((:file "disk")))))))
               (:file "ext"))
  :in-order-to ((test-op (test-op "gui/tests"))))

(defsystem :gui/tests
  :depends-on (:rt :gui)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :gui)))
