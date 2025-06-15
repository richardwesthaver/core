;;; aws-lc.asd --- AWS-LC bindings - libcrypto

;;

;;; Commentary:

;; 

;;; Code:
(defsystem :aws-lc
  :depends-on (:std)
  :components ((:file "pkg")
               (:file "types")
               (:file "condition")
               (:file "aws-lc"))
  :in-order-to ((test-op (test-op "aws-lc/tests"))))

(defsystem :aws-lc/tests
  :depends-on (:rt :aws-lc)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call :rt :do-tests :aws-lc)))
