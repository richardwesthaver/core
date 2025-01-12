;;; srv.lisp --- Homer Services

;; 

;;; Code:
(in-package :homer/core)

(defvar *systemd-config-directory* (merge-homedir-pathnames ".config/systemd/"))

(defclass homer-service (id ast)
  ((engine :initarg :engine :initform nil :type (or null keyword))
   (config :initarg :config))
  (:documentation "Base class for HOMER services. Services are similar to Systemd units - they
may be individually controlled by an ORACLE thread (usually the default
toplevel)."))

(defmethod name ((self homer-service)) (id self))

(defun systemd-service-name (name)
  (format nil "~A.service" (string-downcase name)))

(defun load-systemd-unit-file (name &optional (path (merge-pathnames "user/default.target.wants/" *systemd-config-directory*)))
  (deserialize (merge-pathnames (systemd-service-name name) path) :ini))

(defmethod load-ast ((self homer-service))
  (with-slots (ast) self
    (setf (id self) (pop ast))
    (let ((props (pop ast)))
      (when-let ((engine (getf props :engine)))
        (setf (slot-value self 'engine) engine))
      (when (eql (slot-value self 'engine) :systemd) 
        (setf (slot-value self 'config)
              (load-systemd-unit-file (id self))))
      (log:debug! "loaded service: ~A" (id self))
      self)))

(defmethod build-ast ((self homer-service) &key)
  (unless (equal (id self) (car (ast self)))
    (setf (ast self)
          `(,(id self) (:engine ,(slot-value self 'engine)) ,@(ast self)))))

(defmethod write-ast ((self homer-task) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(id self) (:repeat ,(schedule self)) ,@(ast self)) :stream stream :pretty pretty :case case :readably t :array t :escape t))

(defmethod start ((self homer-service))
  (ecase (slot-value self 'engine)
    (:systemd (cli/tools/systemd:systemctl-start "--user" (systemd-service-name (id self))))
    (nil (eval (ast self)))))

(defun homer-service-start (self &key args)
  (ecase (slot-value self 'engine)
    (:systemd (apply 'cli/tools/systemd:systemctl-start "--user" (systemd-service-name (id self)) args))
    (nil (eval (ast self)))))

(defun homer-service-restart (self &key args)
  (ecase (slot-value self 'engine)
    (:systemd (apply 'cli/tools/systemd:systemctl-restart "--user" (systemd-service-name (id self)) args))
    (nil (eval (ast self)))))
  
(defmethod stop ((self homer-service) &key args)
  (ecase (slot-value self 'engine)
    (:systemd (apply 'cli/tools/systemd:systemctl-stop "--user" (systemd-service-name (id self)) args))
    (nil)))

(defmethod status ((self homer-service) &key args)
  (ecase (slot-value self 'engine)
    (:systemd (apply 'cli/tools/systemd:systemctl-status "--user" (systemd-service-name (id self)) args))
    (nil (describe self))))
