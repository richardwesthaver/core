;;; cuda.asd --- Cuda Sytem Definitions
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage :cuda.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :cuda.sys)

(defsystem :cuda
  :depends-on (:std :log)
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :cuda)
               (:file "cuda")))
