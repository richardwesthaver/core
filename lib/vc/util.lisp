;;; util.lisp --- VC High-level Utils

;; 

;;; Code:
(in-package :vc/util)

(defun find-repo-root (&optional path)
  "Check PATH for evidence of a VCS and continue walking up the filesystem until
we find one, else return NIL."
  (labels ((%check (dir)
             (if (null dir)
                 (return-from find-repo-root)
                 (if (directory (merge-pathnames ".hg/" dir))
                     :hg
                     (when (directory (merge-pathnames ".git/" dir))
                       :git)))))
    (let ((%path (car (directory (or path *default-pathname-defaults*)))))
      (loop for x = (%check %path)
            for parent = (when-let ((parent (butlast (pathname-directory %path))))
                           (make-pathname :directory parent))
            if x
            return (values %path x)
            else if (not parent)
            return nil
            else
            do (setf %path parent)))))

(defun make-hg-repo (path &key init update)
  (let ((repo (make-instance 'hg-repo :path path)))
    (when init (vc-init repo))
    (when update
      (setf (vc/hg::vc-requires repo)
            (mapcar (lambda (s) (trim s))
                    (sb-unicode:lines (vc-run repo "debugrequires")))))
    repo))

(defun make-git-repo (path &key init)
  (let ((repo (make-instance 'git-repo :path path)))
    (when init (vc-init repo))
    repo))

(defun make-repo (path &key (type *default-vc-kind*) init)
  (case type
    (:hg (make-hg-repo path :init init))
    (:git (make-git-repo path :init init))
    (t (error "invalid repo type: ~A" type))))

(defmacro with-current-vc-root ((sym &optional dir) &body body)
  `(let ((,sym
           (multiple-value-bind (root kind) (find-repo-root ,dir)
             (if root 
                 (make-repo root :type kind)
                 (error 'vc-error :message "Directory not under version control")))))
     ,@body))

(defun directory-repos (path)
  (let ((path (probe-file path)))
    (assert (typep path 'directory-pathname))
    (loop for p in (directory (merge-pathnames "*/" path))
          collect (make-repo p))))

(defun bundle-repo (path output)
  (vc-bundle (make-repo path) output))

(defun bundle-repos (path output)
  (loop for repo in (directory-repos path)
        do (let ((out (merge-pathnames output (name repo))))
             (vc-bundle repo out))))

(defun update-repo (repo &optional push (pull t))
  (when pull
    (vc-pull repo (when (stringp pull) pull)))
  (when push
    (vc-push repo (when (stringp push) push))))

(defun update-repos (path &key push (pull t))
  (loop for repo in (directory-repos path)
        do (update-repo repo push pull)))

(defmacro with-repo ((sym &rest args &key path init type &allow-other-keys) &body body)
  `(with-directory ,path
     (let ((,sym ,@(or (unless (keywordp (car args))
                         `(pop ,args))
                       `((make-repo ,path ,@(when init `(:init ,init)) ,@(when type `(:type ,type)))))))
       (setf *repo* ,sym)
       ,@body)))
