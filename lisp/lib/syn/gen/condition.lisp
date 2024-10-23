;;; condition.lisp --- SYN/GEN Core Conditions

;; 

;;; Code:
(in-package :syn/gen)
(eval-always
  (define-condition codegen-condition () ()))
(deferror codegen-error (codegen-condition) () (:auto t))
(deferror simple-codegen-error (codegen-condition simple-error) () (:auto t))
