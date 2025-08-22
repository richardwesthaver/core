;;; cfg.lisp --- Packy Configuration

;; 

;;; Code:
(in-package :skel/packy)

(defconfig packy-config (ast)
  ((path :initarg :path :accessor path)))

(defconfig packy-user-config (packy-config) ())

(defmethod make-config ((self (eql :packy)) &key ast path)
  (make-instance 'packy-user-config :ast ast :path path))
