(defsystem :pod
  :depends-on (:std :cli :obj :dat :net :flexi-streams :dexador)
  :components ((:file "pkg")
               (:file "err")
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
               (:file "api")
               (:file "buildah")
               (:file "podman")
               (:file "client")
               (:file "util"))
  :in-order-to ((test-op (test-op :pod/tests))))

(defsystem :pod/tests
  :depends-on (:rt :pod)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :pod)))
