;;; jpeg.asd --- JPEG Sytem Definitions
(defpackage :jpeg.sys
  (:use :cl :asdf :sb-grovel :sb-alien))
(in-package :jpeg.sys)
(defsystem :jpeg
  :depends-on (:std :log :sb-grovel)
  :description "FFI wrapper for JPEG. Currently binds to libjpeg-turbo."
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :jpeg))
  :in-order-to ((test-op (test-op "jpeg/tests"))))
(defsystem :jpeg/tests
  :depends-on (:std :log :rt :jpeg)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :jpeg)))
