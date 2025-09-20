;;; openssl.asd --- (AWS) Libcrypto  bindings

;; These bindings assume the use of AWS_LC.

;;; Commentary:

;; 

;;; Code:
(defsystem :openssl
  :depends-on (:std)
  :components ((:file "pkg")
               (:file "types")
               (:file "condition")
               (:file "openssl"))
  :in-order-to ((test-op (test-op "openssl/tests"))))

(defsystem :openssl/tests
  :depends-on (:rt :openssl)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call :rt :do-tests :openssl)))
