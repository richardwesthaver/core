;;; homer.lisp --- Homer

;; 

;;; Code:
(in-package :homer)

(defvar *user* (sb-posix:getenv "USER"))
(defvar *user-homedir* (user-homedir-pathname))
(defvar *default-user-homerc* (merge-pathnames ".homerc" *user-homedir*))
(declaim (type home-config *home-config*))
(defvar *home-config*)
(defvar *home-hidden-paths* (nconc *hidden-paths* (list "stash" "store" "readme.org" ".hgignore")))
(defvar *homer-force* nil)

(defun init-homer-vars ()
  (setq *user* (sb-posix:getenv "USER")
        *user-homedir* (user-homedir-pathname)
        *default-user-homerc* (merge-pathnames ".homerc" *user-homedir*)))

(defclass home-config (ast id)
  ((user :initform *user* :initarg :user :type string)
   (path :initform nil :initarg :path :type (or pathname null))
   (src :initform nil :initarg :src :type (or null pathname vc-repo))
   (skel :initform (load-user-skelrc) :initarg :skel :type (or null pathname sk-user-config))
   (krypt :initform (load-kryptrc) :initarg :krypt :type (or null pathname krypt-config))
   (packy :initform nil :initarg :packy :type (or null pathname pk-user-config))
   (mail :initarg :mail :type pathname)
   (term :initform nil :type (or pathname null term-user-config))
   (shell :initarg :shell :type (or pathname shell-user-config))
   (editor :initarg :editor :type (or pathname editor-user-config))
   (wm :initarg :wm :type (or pathname wm-user-config))
   (browser :initarg :browser :type (or pathname browser-user-config))))

(defmethod print-object ((self home-config) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~S ~A" :id (format-sxhash (id:id self)))))

(defun find-homer-symbol (s)
  (find-symbol* (symbol-name s) :homer nil))

(defmethod load-ast ((self home-config))
  (with-slots (ast) self
    (if (ast:formp ast)
        ;; ast is valid, modify object, set ast nil
        (progn
          (sb-int:doplist (k v) ast
            (when-let ((s (find-homer-symbol k))) ;; needs to be correct package
              (setf (slot-value self s) v)))
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

(defun mtime (path) (sb-posix:stat-mtime (sb-posix:stat path)))
(defun ctime (path) (sb-posix:stat-ctime (sb-posix:stat path)))

(defun compare-home-file (src)
  "Compare a SRC path to what is stored in the user's home. Return a cons with
the last modified timestamp of each file (SRC . HOME) or NIL."
  (let* ((name (enough-namestring src))
         (home (merge-pathnames name (user-homedir-pathname)))
         (m1 (mtime src))
         (m2 (when (probe-file home) (mtime home)))
         (status (cond
                   ((null m2) :new)
                   ((> m1 m2) :pull)
                   ((< m1 m2) (unless (= (ctime home) m2)
                                :push))
                   (t))))
    (cons status (cons src home))))

(defun homer-status (file)
  (let ((form (compare-home-file file)))
    (case (car form)
      ;; confirm with user
      (:new (println (format nil ":NEW ~A" (cdr form))))
      (:pull (println (format nil ":PULL ~A" (cadr form))))
      (:push (println (format nil ":PUSH ~A" (cddr form))))
      (t nil))))

(defun homer-copy (input output)
  (ensure-directories-exist output :verbose t)
  (uiop:copy-file input output))

(defun homer-maybe-push (file)
  (let ((form (compare-home-file file)))
    (case (car form)
      (:push (progn
               (println (format nil ":PUSH ~A" (cddr form)))
               (homer-copy (cddr form) (cadr form))))
      (t nil))))

(defun homer-maybe-pull (file)
  (let ((form (compare-home-file file)))
    (case (car form)
      (:pull (progn
               (println (format nil ":PULL ~A" (cddr form)))
               (homer-copy (cadr form) (cddr form))))
      (t nil))))

(defun homer-maybe-install (file)
  (let ((form (compare-home-file file)))
    (case (car form)
      (:pull (progn
               (println (format nil ":PULL ~A" (cddr form)))
               (homer-copy (cadr form) (cddr form))))
      (:new (progn
              (println (format nil ":NEW ~A" (cddr form)))
              (homer-copy (cadr form) (cddr form))))
      (:push (if *homer-force*
                 (progn
                   (println (format nil ":OVERWRITE ~A" (cddr form)))
                   (homer-copy (cadr form) (cddr form)))
                 (trace! "skipping file:" (cddr form))))
      (t nil))))
