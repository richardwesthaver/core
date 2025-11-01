;;; cfg.lisp --- Log Configuration

;; 

;;; Code:
(in-package :log)

(defconfig logger-config (ast)
  ((size :initform 10 :initarg :size)
   (level :initform :info :initarg :level :accessor level)))

(defmethod pipe ((self logger-config))
  (ast self))

(defmethod sink ((self logger-config))
  (last (pipe self)))

(defmethod source ((self logger-config))
  (first (pipe self)))

(defmethod make-config ((self (eql :logger)) &key ast pipe (size 10) (level :info))
  (make-instance 'logger-config :ast (or ast pipe) :size size :level level))

(defun build-logger-config (cfg)
  (apply 'defpipe* (make-instance 'logger) (pipe cfg)))

(defmethod build ((self logger-config) &key)
  (build-logger-config self))

(defun default-logger-config ()
  (let ((cfg (make-config :logger)))
    (setf (ast cfg)
          `((level-filter :id :level-filter :level ,(level cfg))
            (tag-tree-filter :id :tag-filter) 
            (stream-sink :id :sink)))
    cfg))

(defmethod init ((self (eql :log)) &rest args)
  "Initialize the global logger."
  (init-log-timestamp)
  (setq *logger* 
        (build (if (null args)
                   (default-logger-config)
                   (apply 'make-config :logger args)))))
