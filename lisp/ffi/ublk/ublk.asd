;;; ublk.asd --- UBLK SYSTEMS

;;; Code:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage :ublk.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :ublk.sys)

(defsystem "ublk"
  :description "UBLK/C FFI"
  :depends-on (:sb-grovel :std)
  :in-order-to ((test-op (test-op "ublk/tests")))
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :ublk)
               (:file "cmd")
               (:file "srv"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :ublk)))

(defsystem "ublk/tests"
  :depends-on (:rt :ublk)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :ublk)))
