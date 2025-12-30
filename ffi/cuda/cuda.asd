;;; cuda.asd --- Cuda Sytem Definitions
(defsystem :cuda
  :depends-on (:std :log)
  :components ((:file "pkg")
               (sb-grovel:grovel-constants-file "constants"
                                      :package :cuda)
               (:file "type")
               (:file "alien")
               (:file "ctx")
               (:file "mem")
               (:file "event"))
  :in-order-to ((test-op (test-op "cuda/tests"))))

(defsystem :cuda/tests
  :depends-on (:rt :cuda)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call :rt :do-tests :cuda)))
