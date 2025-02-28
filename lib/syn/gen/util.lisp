;;; util.lisp --- Codegen Utilities

;; 

;;; Code:
(in-package :syn/gen)

(defun init-gen (key)
  (etypecase key
    (gen-designator (setq *gen* key))))

(defmacro with-codegen (lang &body body)
  "Enable the *GEN* reader for the duration of BODY."
  `(unwind-protect (progn
                     (load-gen ,lang)
                     (funcall (gen-reader *gen*))
                     ,@body)
     (unload-gen ,lang)
     (init-gen nil)))

