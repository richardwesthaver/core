;;; jpeg.asd --- JPEG Sytem Definitions (libjpeg-turbo)
(defpackage :jpeg-sys
  (:use :cl :asdf :sb-grovel :sb-alien))
(in-package :jpeg-sys)
(defsystem :jpeg
  :depends-on (:std :log)
  :description "FFI wrapper for JPEG. Currently binds to libjpeg-turbo."
  :components ((:file "pkg")
               (grovel-constants-file "constants" :package :jpeg)
               (:file "jpeg")
               (:file "turbo")
               (:file "api"))
  :in-order-to ((test-op (test-op "jpeg/tests"))))
(defsystem :jpeg/tests
  :depends-on (:std :log :rt :jpeg)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :jpeg)))
