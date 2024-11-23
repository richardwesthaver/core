(defsystem :pod
  :depends-on (:std :cli :obj :dat :net :io)
  :components ((:file "pkg")
               (:file "condition")
               (:module "obj"
                :components 
                ((:file "system")
                 (:file "secret")
                 (:file "network")
                 (:file "exec")
                 (:file "volume")
                 (:file "image")
                 (:file "container")
                 (:file "pod")))
               (:file "containerfile")
               (:file "api")
               (:file "podman")
               (:file "client")
               (:file "util"))
  :in-order-to ((test-op (test-op :pod/tests))))

(defsystem :pod/tests
  :depends-on (:rt :pod)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :pod)))
