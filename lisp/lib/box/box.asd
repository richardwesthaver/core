(defsystem :box
  :description "Kernel virtualization support for Lisp - wraps QEMU, LXC, KVM, Libvirt."
  :depends-on (:std :cli :obj :dat :net)
  :components ((:file "pkg"))
  :in-order-to ((test-op (test-op :box/tests))))

(defsystem :box/tests
  :depends-on (:rt :box)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :box)))
