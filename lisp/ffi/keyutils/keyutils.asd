;;; keyutils.asd --- Linux keyutils FFI bindings

;;

;;; Commentary:

;; 

;;; Code:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage :keyutils.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :keyutils.sys)

(defsystem :keyutils
  :depends-on (:sb-grovel :std)
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :keyutils))
  :in-order-to ((test-op (test-op "keyutils/tests"))))

(defsystem :keyutils/tests
  :depends-on (:rt :keyutils)
  :components ((:file "tests"))
  :perform (test-op (op c) (symbol-call :rt :do-tests :keyutils)))
