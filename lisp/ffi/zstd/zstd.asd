;;; uring.asd-*- mode: lisp; -*-
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage :zstd.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :zstd.sys)

(defsystem :zstd
  :depends-on (:sb-grovel :std)
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :zstd))
  :in-order-to ((test-op (test-op :zstd/tests))))

(defsystem :zstd/tests
  :depends-on (:std/rt :zstd)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :zstd)))
