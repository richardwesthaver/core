;;; rustls.asd --- RUSTLS FFI bindings

;;

;;; Commentary:

;; 

;;; Code:
(defsystem :rustls
  :depends-on (:std)
  :components ((:file "pkg")
               (:file "types")
               (:file "rustls"))
  :in-order-to ((test-op (test-op "rustls/tests"))))

(defsystem :rustls/tests
  :depends-on (:rt :rustls)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call :rt :do-tests :rustls)))
