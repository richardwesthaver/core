;;; cfg.lisp --- Homer Config

;; 

;;; Code:
(in-package :skel/homer/core)

(defconfig home-config (ast id)
  ((user :initform (current-user) :initarg :user :type string)
   (path :initform nil :initarg :path :type (or pathname null))
   (src :initform #p"/usr/src/home/" :initarg :src :type (or null pathname vc-repo))
   (skel :initform nil :initarg :skel :type (or null pathname sk-config))
   (krypt :initform nil :initarg :krypt :type (or null pathname krypt-config))
   (mpk :initform nil :initarg :mpk :type (or null pathname mpk-config))
   (packy :initform nil :initarg :packy :type (or null packy-config))
   (logger :initform (default-logger-config) :initarg :logger :type (or null logger-config) :accessor logger)
   (mail :initarg :mail :type pathname)
   (term :initform nil :type (or pathname null term-config))
   (tmux :initform nil :type (or pathname null tmux-config))
   (shell :initarg :shell :type (or pathname shell-config))
   (tasks :initarg :tasks :type list :accessor tasks)
   (jobs :initarg :jobs :type list :accessor jobs)
   (editor :initarg :editor :type (or pathname editor-config))
   (browser :initarg :browser :type (or pathname browser-config))
   (keyboard :initarg :keyboard :type (or pathname keyboard-config))
   (boxes :initarg :box :type list)
   (pods :initform nil :initarg :pod :type list)
   (services :initform nil :initarg :services :type list :accessor services)))

(defmethod make-config ((self (eql :homer)) &rest args)
  (apply 'make-instance 'home-config args))

(defmethod print-object ((self home-config) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~S ~A" :id (format-sxhash (id:id self)))))

(defun find-homer-symbol (s)
  (find-symbol* (symbol-name s) :skel/homer/core nil))

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
                        (:logger (apply 'make-config :logger v))
                        (:term (make-config :term :ast v))
                        (:tmux (apply 'make-config :tmux v))
                        (:editor (if (atom v)
                                     (make-config :editor :type v)
                                     (apply 'make-config v)))
                        (:pods (mapcar (lambda (x) (apply 'make-config :pod :name x)) v))
                        (:boxes (mapcar (lambda (x) (apply 'make-config :box :name x)) v))
                        (:packy 
                         (if (atom v)
                             (make-config :packy :path v)
                             (apply 'make-config :packy ast)))
                        (:tasks 
                         (let ((ret))
                           (dolist (task v ret)
                             (push (load-ast (make-instance 'homer-task :ast task)) ret))))
                        (:jobs
                         (let ((ret))
                           (dolist (job v ret)
                             (push 
                              (make-homer-job :target (string (pop job)) :source (pop job) :recipe job)
                              ret))))
                        (:services
                         (let ((ret))
                           (dolist (srv v ret)
                             (push 
                              (load-ast (make-instance 'homer-service :ast srv))
                              ret))))
                        (t v)))
                (setf (slot-value self s) v))))
          (unless *keep-ast* (setf (ast self) nil))
          self)
        ;; invalid ast, signal error
        (error 'syntax-error))))

;; obj -> ast
(defmethod build ((self home-config) &key (nullp nil) (exclude '(ast id)))
  (setf (ast:ast self)
        (ast:unwrap-object self
                       :slots t
                       :methods nil
                       :nullp nullp
                       :exclude exclude)))

(defun init-homerc (&optional (file *user-homerc*))
  (let ((cfg (make-instance 'home-config)))
    (build cfg :exclude '(ast id skel krypt #+mpk mpk logger))
    (with-open-file (out file
                         :direction :output
                         :if-does-not-exist :create)
      (write-ast cfg out :fmt :canonical))))

(defun load-homerc (&optional (file *user-homerc*) (init t))
  "Load a homerc configuration from FILE. Defaults to ~/.homerc."
  (flet ((%load ()
           (with-readtable :shell
             (let ((form
                     (file-read-forms file)))
               (setq *home-config* (load-ast (make-instance 'home-config :ast form :path file :id (sxhash form))))
               (with-slots (src) *home-config*
                 (if src
                     (setf src (pathname src))
                     (if-let ((homer (sb-posix:getenv "HOMER")))
                       (setf src (pathname homer))
                       (error "missing HOMER directory"))))))))
    (if (not init)
        (progn 
          (assert (probe-file file))
          (%load))
        (if (probe-file file)
            (%load)
            (init-homerc file))))
  (setf *log-level* (level (logger *home-config*)))
  *home-config*)
