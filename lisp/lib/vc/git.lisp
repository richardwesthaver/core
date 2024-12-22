(in-package :vc/git)

(deferror git-error (vc-error) () (:auto t))

(defvar *git-program* (cli:find-exe "git"))

(defun run-git-command (cmd &optional args (output t) (wait t))
  (unless (listp args) (setf args (list args)))
  (setf args (mapcar #'vc/proto::namestring-or args)) ;;  TODO 2024-05-10: slow
  (sb-ext:run-program *git-program* (push cmd args) :output output :wait wait :input nil))

(defun git-url-p (url)
  "Return nil if URL does not look like a URL to a git valid remote."
  (let ((url-str (if (typep url 'pathname)
                     (namestring url)
                     url)))
    (ppcre:scan '(:alternation
            (:regex "\\.git$")
            (:regex "^git://")
            (:regex "^https://git\\.")
            (:regex "^git@"))
          url-str)))

(defun gitignore (&optional (path ".gitignore"))
  (vc/proto::make-vc-ignore :path path :patterns (vc/proto::map-lines #'vc/proto::glob-path-match path)))

;; https://git-scm.com/docs/git-config
(defclass git-config (vc-config) ())

;; TODO 2024-08-22: read ini files
(defmethod find-config ((obj (eql :git)) &rest args &key (directory (user-homedir-pathname)))
  (declare (ignore args))
  (let ((*default-pathname-defaults* directory))
    (when-let ((config (directory ".gitconfig")))
      (car config))))

(defclass git-repo (vc-repo)
  ((index))) ;; working-directory

(defmethod vc-type ((self git-repo)) :git)

(defmethod print-object ((self git-repo) stream)
  (print-unreadable-object (self stream)
    (format stream "~S" (vc-type self))
    (when-let ((remotes (vc-remotes self)))
      (format stream " ~A" remotes))))

(defmethod vc-init ((self (eql :git)))
  (make-instance 'git-repo :path (pathname *default-pathname-defaults*)))

(defmethod vc-init ((self git-repo))
  (let ((path (path self)))
    (if (zerop (sb-ext:process-exit-code (run-git-command "init" path)))
        (not (probe-file path))
        (git-error "git init failed:" path))))

(defmethod vc-run ((self git-repo) (cmd string) &rest args)
  (with-directory (path self)
    (run-git-command cmd args)))

(defmethod vc-clone ((self git-repo) remote &key &allow-other-keys)
  (with-slots (path) self
    (sb-ext:process-exit-code (run-git-command "clone" remote path))))

(defmethod vc-pull ((self git-repo) &optional (remote "main"))
  (with-slots (path) self
    (with-directory path
      (sb-ext:process-exit-code (run-git-command "pull" remote)))))

(defmethod vc-push ((self git-repo) &optional (remote "main"))
  (with-slots (path) self
    (with-directory path
      (sb-ext:process-exit-code (run-git-command "push" remote)))))

(defmethod vc-commit ((self git-repo) msg &key &allow-other-keys)
  (with-slots (path) self
    (with-directory path
      (sb-ext:process-exit-code (run-git-command "commit" "-m" msg)))))

(defmethod vc-add ((self git-repo) &rest files)
  (with-slots (path) self
    (with-directory path
      (sb-ext:process-exit-code (apply #'run-git-command "add" files)))))

(defmethod vc-remove ((self git-repo) &rest files)
  (with-slots (path) self
    (with-directory path
      (sb-ext:process-exit-code (apply #'run-git-command "remove" files)))))

;; TODO
(defmethod vc-addremove ((self git-repo) &rest files)
  (with-slots (path) self
    (with-directory path
      (sb-ext:process-exit-code (apply #'run-git-command "addremove" files)))))

(defmethod vc-status ((self git-repo) &key &allow-other-keys) (vc-run self "status"))

(defmethod vc-branch ((self git-repo)) (vc-run self "branch"))

(defmethod vc-diff ((a git-repo) (b git-repo) &key &allow-other-keys)
  (vc-run a "diff" (vc-head a) (vc-head b)))

(defmethod id ((self git-repo))
  (with-slots (path) self
    (with-directory path
      (with-open-stream (s (sb-ext:process-output (run-git-command "id")))
        (with-output-to-string (str)
          (loop for c = (read-char s nil nil)
                while c
                do (write-char c str))
          str)))))

;; TODO 2023-12-29: does git have a cmdserver?
;; (declaim (inline make-git-client))
;; (defstruct git-client
;;   (pid 0 :type fixnum :read-only t)
;;   (pgid 0 :type fixnum)
;;   (cwd (sb-posix:getcwd) :type string))
