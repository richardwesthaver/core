;;; util.lisp --- Codegen Utilities

;; 

;;; Code:
(in-package :syn/gen)

(defun init-gen (key)
  (etypecase key
    (gen-designator (setq *gen* key))))

(defmacro with-codegen ((sym &rest args &key &allow-other-keys) &body body)
  (declare (ignorable args))
  `(with-package (generator-package (init-gen ,sym))
     (funcall (generator-reader ,sym))
     ,@body
     (cl-reader)))
