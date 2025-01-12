;;; log.lisp --- Skel Logger

;; 

;;; Code:
(in-package :skel/log)

(defclass sk-log-schema (rdb-log-schema sk-schema) ())

(defvar *skel-log-schema* (make-instance 'sk-log-schema))

(defclass skel-db-sink (rdb-sink) ()
  (:default-initargs
   :db (make-db :rocksdb :path (skel-db-path "log/") :opts (default-rdb-opts))))

(defmethod initialize-instance :after ((self skel-db-sink) &key (schema *skel-log-schema*))
  (load-schema self schema))

(defvar *skel-logger-config*
  (config:make-config :logger :ast '((level-filter :id :level-filter)
                                     (tag-tree-filter :id :tag-filter)
                                     (skel-db-sink :id :sink))))

(defvar-unbound *skel-logger*)

(defun sk-log (level fmt &rest args)
  (msg *skel-logger* (make-instance 'log-message 
                       :level level 
                       :timestamp (time:now) 
                       :content (format nil fmt args))))

(defun init-skel-logger (&optional (cfg *skel-logger-config*))
  (unless (and (boundp '*skel-logger*) *skel-logger*)
    (let* ((lgr (build cfg))
           (db (sink lgr)))
      (if (probe-file (name db))
          (progn
            (load-opts db)
            (open-columns* db))
          (progn
            (open-columns* db)))
      (setq *skel-logger* lgr))))

;; (funcall 'init-skel-db-logger)
(defun sk-log-list (&optional level)
  (with-db (db :db (sink *skel-logger*) :open nil :close nil)
    (with-iter (it (iter db :column (find-column level db)))
      (seek-to-first)
      (loop while (iter-valid-p)
            collect (cons (time:octets-to-timestamp (key)) 
                          (sb-ext:octets-to-string (val)))
            do (next)))))

(defun sk-log-repair ()
  (if (and (boundp '*skel-logger*) *skel-logger*)
      (repair-db (sink *skel-logger*))
      (with-db (db :db (make-instance 'skel-db-sink) :open nil :close t)
        (repair-db db)
        (open-columns* db))))

(defun sk-log-shutdown ()
  (when (and (boundp '*skel-logger*) *skel-logger*)
    (shutdown-db (sink *skel-logger*))
    (setq *skel-logger* nil)))

(defun sk-log-close ()
  (when (and (boundp '*skel-logger*) *skel-logger*)
    (close-db (sink *skel-logger*))))

;; (sk-log-repair)
;; (init-skel-logger)
;; (sk-log-close)
;; (sk-log-destroy)

(defun sk-log-destroy ()
  (destroy-db (make-instance 'skel-db-sink)))

;; (inspect (create-co (make-instance 'skel-db-sink)))

;; (sk-log-repair)
