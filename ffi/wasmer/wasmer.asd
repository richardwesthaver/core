;;; wasmer.asd --- WASMER FFI bindings

;;

;;; Commentary:

;; 

;;; Code:
(defsystem :wasmer
  :depends-on (:std)
  :components ((:file "pkg")
               (:file "types")
               (:file "wasmer"))
  :in-order-to ((test-op (test-op "wasmer/tests"))))

(defsystem :wasmer/tests
  :depends-on (:rt :wasmer)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call :rt :do-tests :wasmer)))
