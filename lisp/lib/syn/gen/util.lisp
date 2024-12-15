;;; util.lisp --- Codegen Utilities

;; 

;;; Code:
(in-package :syn/gen)

(defun init-gen (key)
  (etypecase key
    (gen-designator (setq *gen* (load-generator key)))))

