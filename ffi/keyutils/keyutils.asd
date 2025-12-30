;;; keyutils.asd --- Linux keyutils FFI bindings

;;

;;; Commentary:

;; 

;;; Code:
(defsystem :keyutils
  :depends-on (:std)
  :components ((:file "pkg")
               (sb-grovel:grovel-constants-file "constants"
                                      :package :keyutils)
               (:file "keyutils" :depends-on ("pkg" "constants"))
               (:file "keyctl" :depends-on ("keyutils")))
  :in-order-to ((test-op (test-op "keyutils/tests"))))

(defsystem :keyutils/tests
  :depends-on (:rt :keyutils)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call :rt :do-tests :keyutils)))
