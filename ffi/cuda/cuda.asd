;;; cuda.asd --- Cuda Sytem Definitions
(defpackage :cuda.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :cuda.sys)

(defsystem :cuda
  :depends-on (:std :log)
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :cuda)
               (:file "type")
               (:file "alien")
               (:file "ctx")
               (:file "mem"))
  :in-order-to ((test-op (test-op "cuda/tests"))))

(defsystem :cuda/tests
  :depends-on (:rt :cuda)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call :rt :do-tests :cuda)))
