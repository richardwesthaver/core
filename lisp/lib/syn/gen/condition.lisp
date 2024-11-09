;;; condition.lisp --- SYN/GEN Core Conditions

;; 

;;; Code:
(in-package :syn/gen)

(eval-always
  (define-condition gen-condition () ()))

(deferror gen-error (gen-condition) () (:auto t))
(deferror simple-gen-error (gen-condition simple-error) () (:auto t))
