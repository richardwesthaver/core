(defsystem :math
  :version "0.1.0"
  :description "Core Math System"
  :depends-on (:std :obj :blas :cuda :syn)
  :components ((:file "pkg")
               (:file "util")
               (:module "blas"
                :components 
                ((:file "axpy")
                 (:file "sum")
                 (:file "gem")
                 (:file "ger")
                 (:file "trs")
                 (:file "norm")))
               (:module "lapack"
                :if-feature :lapack
                :components
                ((:file "lu")
                 (:file "chol")
                 (:file "eig")
                 (:file "lstsq")
                 (:file "qr")
                 (:file "schur")
                 (:file "svd")
                 (:file "syl")
                 (:file "poly")))
               (:module "cuda" 
                :components ((:file "tensor")
                             (:file "kernel")))
               (:file "sym")
               (:file "linfix")
               (:file "syn")
               (:file "readtable")
               (:file "sfc")
               (:file "auto")
               (:file "math"))
  :in-order-to ((test-op (test-op :math/tests))))

(defsystem :math/tests
  :depends-on (:rt :math :log :obj)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :math)))

