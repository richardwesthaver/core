;;; srv.lisp --- Homer Services

;; 

;;; Code:
(in-package :homer/core)

(defvar *systemd-config-directory* (merge-homedir-pathnames ".config/systemd/"))

(defconfig homer-service-config (ast id) ()
  (:documentation "Configuration class for HOMER-SERVICE objects."))

;; https://www.freedesktop.org/software/systemd/man/latest/systemd.unit.html
(defconfig systemd-service-config (homer-service-config) 
  ((unit)
   (service)
   (install))
  (:documentation "HOMER-SERVICE configuration from systemd unit files."))

(defclass homer-service (service ast id)
  ((engine :initarg :engine :initform nil)
   (config :initarg :config :type homer-service-config))
  (:default-initargs 
   :request-class 'homer-service-request
   :response-class 'homer-service-response)
  (:documentation "Base class for HOMER services. Services are similar to Systemd units - they
may be individually controlled by an ORACLE thread (usually the default
toplevel)."))

(defmethod name ((self homer-service)) 
  (typecase (engine self)
    ((eql :systemd) (systemd-service-name (id self)))
    (t (id self))))

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

(defmethod write-ast ((self homer-service) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(id self) (:engine ,(slot-value self 'engine)) ,@(ast self)) :stream stream :pretty pretty :case case :readably t :array t :escape t))

(definline systemd-start (self &optional args)
  (apply 'cli/tools/sys:systemctl-start "--user" `(,(systemd-service-name (id self)) ,@args)))

(definline systemd-restart (self &optional args)
  (apply 'cli/tools/sys:systemctl-restart "--user" `(,(systemd-service-name (id self)) ,@args)))

(definline systemd-stop (self &optional args)
  (apply 'cli/tools/sys:systemctl-stop "--user" `(,(systemd-service-name (id self)) ,@args)))

(definline systemd-status (self &optional args)
  (apply 'cli/tools/sys:systemctl-status "--user" `(,(systemd-service-name (string-downcase (id self))) ,@args)))

(defmethod start ((self homer-service))
  (case (slot-value self 'engine)
    (:systemd (systemd-start self))
    (t (eval (ast self)))))

(defmethod reset ((self homer-service) &rest args)
  (case (slot-value self 'engine)
    (:systemd (systemd-restart self args))
    (t)))

(defmethod stop ((self homer-service) &key args)
  (case (slot-value self 'engine)
    (:systemd (systemd-stop self args))
    (t)))

(defmethod status ((self homer-service) &key args)
  (case (print (slot-value self 'engine))
    (:systemd (print (systemd-status self args)))
    (t (describe self))))
