(defsystem :box
  :description "Kernel virtualization support for Lisp - wraps QEMU,archiso,etc."
  :depends-on (:std :cli :obj :dat :net :log)
  :components ((:file "pkg")
               (:file "proto")
               (:file "archiso")
               (:file "qmp")
               (:file "qga"))
  :in-order-to ((test-op (test-op :box/tests))))

(defsystem :box/tests
  :depends-on (:rt :box :log)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :box)))
