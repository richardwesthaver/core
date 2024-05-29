;;; readline.asd --- GNU Readline FFI bindings

;;

;;; Commentary:

;; 

;;; Code:
(defsystem :readline
  :depends-on (:std)
  :components ((:file "pkg"))
  :in-order-to ((test-op (test-op "readline/tests"))))

(defsystem :readline/tests
  :depends-on (:rt :readline)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call :rt :do-tests :readline)))
