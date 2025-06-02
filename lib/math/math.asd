(defsystem :math
  :version "0.1.0"
  :description "Core Math System"
  :depends-on (:std :obj :blas :cuda)
  :components ((:file "pkg")
               (:file "var")
               (:file "proto")
               (:file "prim")
               (:file "sfc")
               (:file "auto")
               (:file "readtable"))
  :in-order-to ((test-op (test-op :math/tests))))

(defsystem :math/tests
  :depends-on (:rt :math :log :obj)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :math)))

