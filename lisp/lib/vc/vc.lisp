;;; vc/vc.lisp --- VC API

;; High-level API for working with VC objects.

;;; Code:
(in-package :vc)

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

(defun directory-repos (path)
  (let ((path (probe-file path)))
    (assert (typep path 'directory-pathname))
    (loop for p in (directory (merge-pathnames "*/" path))
          collect (make-repo p))))

(defun bundle-repo (path output)
  (vc-bundle (make-repo path) output))

(defun bundle-repos (path output)
  (loop for repo in (directory-repos path)
        do (let ((out (merge-pathnames output (vc-name repo))))
             (vc-bundle repo out))))

(defun update-repo (repo &optional push (pull t))
  (when pull
    (vc-pull repo (when (stringp pull) pull)))
  (when push
    (vc-push repo (when (stringp push) push))))

(defun update-repos (path &key push (pull t))
  (loop for repo in (directory-repos path)
        do (update-repo repo push pull)))

(defmacro with-repo ((sym &rest args &key path init &allow-other-keys) &body body)
  `(let ((,sym ,@(or (unless (keywordp (car args)) 
                       `(pop ,args))
                     `((make-repo ,path :init ,init)))))
     (setf *repo* ,sym)
     ,@body))
