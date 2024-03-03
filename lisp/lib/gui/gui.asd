;;; gui.asd --- GUI subsystem
(defsystem :gui
  :depends-on (:std 
               :log :obj :xkb
               :wayflan :wayflan-client ;;#+wl
               :clx :stumpwm) ;; #+x11
  :components ((:file "pkg")
               (:file "err")
               (:module "wm"
                :components 
                ((:file "pkg")
                 (:module "wl"
                  :components 
                  ((:file "pkg")
                   (:file "kbd")
                   (:file "shell")))
                 (:module "x11"
                  :components ((:file "pkg")))))
               (:file "ext"))
  :in-order-to ((test-op (test-op "gui/tests"))))

(defsystem :gui/tests
  :depends-on (:rt :gui)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :gui)))
