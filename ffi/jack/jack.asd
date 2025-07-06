;;; ffi/jack/jack.asd --- Jack Sytem Definitions
(defsystem :jack
  :depends-on (:std :log :dat)
  :components ((:file "pkg"))
  :in-order-to ((test-op (test-op "jack/tests"))))
(defsystem :jack/tests
  :depends-on (:rt :jack)
  :components ((:file "tests"))
  :perform (test-op (o c) (uiop:symbol-call :rt :do-tests :jack)))
               
