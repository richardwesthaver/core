;;; homer.lisp --- homectl utility

;;; Code:
(defpackage :bin/homer
  (:nicknames :homer)
  (:use :cl :std :log :sxp :rdb :skel :packy :cli :obj/id :krypt :vc)
  (:export :main :home-config))

(in-package :bin/homer)
(defvar *user* (sb-posix:getenv "USER"))
(defvar *user-homedir* (user-homedir-pathname))
(defvar *default-user-homerc* (merge-pathnames ".homerc" *user-homedir*))
(declaim (type home-config *home-config*))
(defvar *home-config*)

(defclass home-config (sxp id)
  ((user :initform *user* :initarg :user :type string)
   (path :initform nil :initarg :path :type (or pathname null))
   (src ::initform nil :initarg :src :type (or null pathname vc-repo))
   (skel :initform (load-user-skelrc) :initarg :skel :type (or null pathname sk-user-config))
   (krypt :initform (load-kryptrc) :initarg :krypt :type (or null pathname krypt-config))
   (packy :initform nil :initarg :packy :type (or null pathname pk-user-config))
   (mail :initarg :mail :type pathname)
   (shell :initarg :shell :type (or pathname shell-user-config))
   (editor :initarg :editor :type (or pathname editor-user-config))
   (wm :initarg :wm :type (or pathname wm-user-config))
   (browser :initarg :browser :type (or pathname browser-user-config))))


(defmethod print-object ((self home-config) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~S ~A" :id (format-sxhash (id self)))))

(defun find-homer-symbol (s)
  (find-symbol* (symbol-name s) :homer nil))

(defmethod load-ast ((self home-config))
  (with-slots (ast) self
    (if (formp ast)
        ;; ast is valid, modify object, set ast nil
        (progn
          (sb-int:doplist (k v) ast
            (when-let ((s (find-homer-symbol k))) ;; needs to be correct package
              (setf (slot-value self s) v)))
          (setf (ast self) nil)
          self)
        ;; invalid ast, signal error
        (error 'sxp-syntax-error))))

;; obj -> ast
(defmethod build-ast ((self home-config) &key (nullp nil) (exclude '(ast id)))
  (setf (ast self)
         (unwrap-object self
                        :slots t
                        :methods nil
                        :nullp nullp
                        :exclude exclude)))
        
(defun load-homerc (&optional (file *default-user-homerc*))
  "Load a homerc configuration from FILE. Defaults to ~/.homerc."
  (unless (null (probe-file file))
    (let ((form (file-read-forms file)))
      (setq *home-config* (load-ast (make-instance 'home-config :ast form :path file :id (sxhash form))))
      (with-slots (src) *home-config*
        (if (info! src)
            (setf src (pathname src))
            (setf src (pathname (sb-posix:getenv "HOMER"))))))))

;;; CLI
(defopt homer-help (print-help $cli))
(defopt homer-version (print-version $cli))
(defopt homer-log-level (when $val (setq *log-level* :debug)))

(defcmd homer-show
  (describe *home-config*))

(defun mtime (path) (sb-posix:stat-mtime (sb-posix:stat path)))

(defun compare-to-home (src)
  "Compare a SRC path to what is stored in the user's home. Return a cons with
the last modified timestamp of each file (SRC . HOME) or NIL."
  (let* ((name (enough-namestring src))
         (home (merge-pathnames name (user-homedir-pathname)))
         (m1 (mtime src))
         (m2 (when (probe-file home) (mtime home)))
         (status (if (null m2) :new
                     (if (< m2 m1) :pull
                         :push))))
    (cons name (list status m1 m2))))

(defcmd homer-check
  (let ((cfg *home-config*))
    (with-slots (src) cfg
      (if-let ((src (probe-file src)))
        (let ((*default-pathname-defaults* src))
          (debug!
           (mapcar #'compare-to-home
                   (std/file:find-files *default-pathname-defaults* (push "readme.org" *hidden-paths*)))))
        (error 'file-error :pathname src)))))

(defcmd homer-push)
(defcmd homer-pull)
(defcmd homer-clean)

(define-cli $cli
  :name "homer"
  :version "0.1.0"
  :description "user home manager"
  :thunk homer-show
  :opts (make-opts
          (:name "level" :global t :description "set the log level" :thunk homer-log-level)
          (:name "help" :global t :description "print help" :thunk homer-help)
          (:name "version" :global t :description "print version" :thunk homer-version))
  :cmds (make-cmds
          (:name show :thunk homer-show)
          (:name check :thunk homer-check)))

(defun run ()
  (let ((*log-level* :info))
    (with-cli (opts cmds args) $cli
      (load-homerc)
      (do-cmd $cli)
      (debug-opts $cli))))

(defmain ()
  (let ((*print-readably* t))
    (run)
    (sb-ext:exit :code 0)))
