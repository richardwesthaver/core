;;; zstd.asd -*- mode: lisp; -*-
(defpackage :zstd.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :zstd.sys)

(defsystem :zstd
  :depends-on (:std)
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :zstd)
               (:file "simple")
               (:file "stream")
               (:file "dict")
               (:file "util"))
  :in-order-to ((test-op (test-op :zstd/tests))))

(defsystem :zstd/tests
  :depends-on (:rt :zstd)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :zstd)))
