;;; keyutils.asd --- Linux keyutils FFI bindings

;;

;;; Commentary:

;; 

;;; Code:
(defpackage :keyutils.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :keyutils.sys)

(defsystem :keyutils
  :depends-on (:sb-grovel :std)
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :keyutils)
               (:file "keyutils" :depends-on ("pkg" "constants"))
               (:file "keyctl" :depends-on ("keyutils")))
  :in-order-to ((test-op (test-op "keyutils/tests"))))

(defsystem :keyutils/tests
  :depends-on (:rt :keyutils)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call :rt :do-tests :keyutils)))
