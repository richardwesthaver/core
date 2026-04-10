;;; cfg.lisp --- Log Configuration

;; 

;;; Code:
(in-package :log)

(defconfig logger-config (ast)
  ((size :initform 10 :initarg :size)
   (level :initform *log-level* :initarg :level :accessor level)))

(defmethod pipe ((self logger-config))
  (ast self))

(defmethod sink ((self logger-config))
  (last (pipe self)))

(defmethod source ((self logger-config))
  (first (pipe self)))

(defmethod make-config ((self (eql :logger)) &key ast pipe (size 10) (level *log-level*))
  (make-instance 'logger-config :ast (or ast pipe) :size size :level level))

(defun build-logger-config (cfg)
  (apply 'defpipe* (make-instance 'logger 
                     :queue (make-array (slot-value cfg 'size) :fill-pointer 0)
                     :queue-back (make-array (slot-value cfg 'size) :fill-pointer 0))
                     (pipe cfg)))

(defmethod build ((self logger-config) &key)
  (build-logger-config self))

(defun default-logger-config (&optional (sink '(stream-sink :id :sink)))
  (let ((cfg (make-config :logger)))
    (setf (ast cfg)
          `((level-filter :id :level-filter :level ,(level cfg))
            ;; (tag-tree-filter :id :tag-filter) 
            ,sink))
    cfg))

(defmethod init ((self (eql :log)) &rest args &key (start t) (timestamp *log-timestamp*) (indent *log-indent*) (level *log-level*) (timestamp-format *log-timestamp-format*) (backtrace *log-show-backtrace*) (message-class *log-message-class*) (message-format *log-message-format*) logger)
  "Initialize the global logger."
  ;; don't remove level, used by MAKE-CONFIG
  (setf args (remove-from-plist args :start :timestamp :indent :timestamp-format :message-class :backtrace :message-format :logger :level))
  (prog1
      (setq 
       *log-show-backtrace* backtrace
       *log-message-class* message-class
       *log-message-format* message-format
       *log-level* level
       *log-timestamp-format* timestamp-format
       *log-timestamp* timestamp
       *log-indent* indent
       *logger* (or logger
                    (build
                     (if (null args)
                         (default-logger-config)
                         (apply 'make-config :logger args)))))
    (when start 
      (start *logger*)
      (info! "logger initialized"))))
