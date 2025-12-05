(defsystem :math
  :version "0.1.0"
  :description "Core Math System"
  :depends-on (:std :obj :blas :cuda :syn)
  :components ((:file "pkg")
               (:file "var")
               (:file "proto")
               (:file "util")
               (:file "sfc")
               (:file "auto")
               (:module "blas"
                :components 
                ((:file "axpy")))
               (:module "lapack"
                :if-feature :lapack
                :components
                ((:file "lu")))
               (:file "linfix")
               (:file "syn")
               (:file "math"))
  :in-order-to ((test-op (test-op :math/tests))))

(defsystem :math/tests
  :depends-on (:rt :math :log :obj)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :math)))

