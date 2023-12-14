;;; gui.asd --- GUI subsystem
(defsystem :gui
  :depends-on (:std :log)
  :components ((:file "pkg"))
  :in-order-to ((test-op (test-op "std/tests"))))
