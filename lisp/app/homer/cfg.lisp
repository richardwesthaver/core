;;; cfg.lisp --- Homer Config

;; 

;;; Code:
(in-package :homer/core)

(defconfig home-config (ast:ast id:id)
  ((user :initform *user* :initarg :user :type string)
   (path :initform nil :initarg :path :type (or pathname null))
   (src :initform nil :initarg :src :type (or null pathname vc-repo))
   (skel :initform (load-user-skelrc) :initarg :skel :type (or null pathname sk-config))
   (krypt :initform (load-kryptrc) :initarg :krypt :type (or null pathname krypt-config))
   (mpk :initform (load-mpkrc) :initarg :mpk :type (or null pathname mpk-config))
   (packy :initform nil :initarg :packy :type (or null pathname packy-config))
   (logger :initform (default-logger-config) :initarg :logger :type (or null logger-config))
   (mail :initarg :mail :type pathname)
   (term :initform nil :type (or pathname null term-config))
   (tmux :initform nil :type (or pathname null tmux-config))
   (shell :initarg :shell :type (or pathname shell-config))
   (editor :initarg :editor :type (or pathname editor-config))
   (wm :initarg :wm :type (or pathname wm-config))
   (browser :initarg :browser :type (or pathname browser-config))
   (keyboard :initarg :keyboard :type (or pathname keyboard-config))
   (boxes :initarg :box :type (or pathname box-config))
   (pods :initarg :pod :type (vector (or pathname pod-config)))
   (services :initarg :services :type (vector (or pathname service-config)))))

(defmethod make-config ((self (eql :home)) &rest args)
  (apply 'make-instance 'home-config args))

(defmethod print-object ((self home-config) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~S ~A" :id (format-sxhash (id:id self)))))

(defun find-homer-symbol (s)
  (find-symbol* (symbol-name s) :homer/core nil))

(defmethod load-ast ((self home-config))
  (with-slots (ast) self
    (if (formp ast)
        ;; ast is valid, modify object, set ast nil
        (progn
          (sb-int:doplist (k v) ast
            (when-let ((s (find-homer-symbol k))) ;; needs to be correct package
              (unless (null v)
                (setf v
                      (case k
                        (:logger (make-config :logger :ast v))
                        (:term (make-config :term :ast v))
                        (:tmux (apply 'make-config :tmux v))
                        (:editor (apply 'make-config :editor v))
                        (t v)))
                (setf (slot-value self s) v))))
          (setf (ast:ast self) nil)
          self)
        ;; invalid ast, signal error
        (error 'syntax-error))))

;; obj -> ast
(defmethod build-ast ((self home-config) &key (nullp nil) (exclude '(ast id)))
  (setf (ast:ast self)
        (ast:unwrap-object self
                       :slots t
                       :methods nil
                       :nullp nullp
                       :exclude exclude)))

(defun load-homerc (&optional (file *default-user-homerc*))
  "Load a homerc configuration from FILE. Defaults to ~/.homerc."
  (unless (null (probe-file file))
    (let ((form
            (sxp:file-read-forms file)))
      (setq *home-config* (load-ast (make-instance 'home-config :ast form :path file :id (sxhash form))))
      (with-slots (src) *home-config*
        (if src
            (setf src (pathname src))
            (if-let ((homer (sb-posix:getenv "HOMER")))
              (setf src (pathname homer))
              (error "missing HOMER directory")))))))
