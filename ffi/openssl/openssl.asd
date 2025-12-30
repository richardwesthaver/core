;;; openssl.asd --- Libssl/Libcrypto  bindings

;; These bindings are not backward compatible with old versions of
;; OpenSSL. Only the latest v3+ is supported.

;;; Commentary:

;; 

;;; Code:
(defsystem :openssl
  :depends-on (:std :io)
  :components ((:file "pkg")
               (sb-grovel:grovel-constants-file "constants" :package :openssl)
               (:file "types")
               (:file "openssl")
               (:file "condition"))
  :in-order-to ((test-op (test-op "openssl/tests"))))

(defsystem :openssl/tests
  :depends-on (:rt :openssl)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call :rt :do-tests :openssl)))
