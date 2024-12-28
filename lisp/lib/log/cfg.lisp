;;; cfg.lisp --- Log Configuration

;; 

;;; Code:
(in-package :log)

(defconfig logger-config (ast)
  ((ast :initform nil :initarg :ast :accessor ast)
   (size :initform 10 :initarg :size)))

(defmethod pipe ((self logger-config))
  (ast self))

(defmethod make-config ((self (eql :logger)) &key ast pipe (size 10))
  (make-instance 'logger-config :ast (or ast pipe) :size size))

(defun build-logger-config (cfg)
  (apply 'defpipe* (make-instance 'logger) (pipe cfg)))
                 
(defmethod build ((self logger-config) &key)
  (build-logger-config self))

(defun default-logger-config ()
  (make-config :logger :ast '((level-filter :id :level-filter) (tag-tree-filter :id :tag-filter) (stream-sink :id :sink))))
