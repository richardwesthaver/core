;;; cfg.lisp --- Log Configuration

;; 

;;; Code:
(in-package :log)

(defconfig logger-config (ast)
  ((ast :initform nil :initarg :ast :accessor ast)
   (size :initform 10 :initarg :size)))

(defmethod pipe ((self logger-config))
  (ast self))

(defmethod make-config ((self (eql :logger)) &key pipe (size 10))
  (make-instance 'logger-config :ast pipe :size size))

(defun build-logger-config (cfg)
  (make-instance 'logger :pipe (apply 'defpipe* (pipe cfg))))
                 
(defmethod build ((self logger-config) &key)
  (build-logger-config self))

;; (build (make-config :logger :pipe '((level-filter))))
(defun default-logger-config ()
  (make-config :logger :ast '(level-filter tag-tree-filter stream-sink)))
