;;; cfg.lisp --- Packy Configuration

;; 

;;; Code:
(in-package :skel/packy)

(defconfig packy-config (ast)
  ((path :initarg :path :accessor path)))

(defconfig packy-user-config (packy-config) ())

(defmethod make-config ((self (eql :packy)) &key ast path)
  (make-instance 'packy-user-config :ast ast :path path))

(defmethod load-ast ((self packy-config))
  (with-slots (ast) self
    (if (formp ast)
        (progn
          (sb-int:doplist (k v) ast
            (when-let ((s (find-symbol (symbol-name k) :skel/packy))) ;; needs to be correct package
              (setf (slot-value self s) v)))
          (unless *keep-ast* (setf (ast self) nil))
          self)
        ;; invalid ast, signal error
        (error 'syntax-error))))

(defmethod load-config ((self (eql :packy)) (from pathname) &key)
  (let ((c (make-config :packy)))
    (load-config c from)))
