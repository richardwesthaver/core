;;; vc/vc.lisp --- VC API

;; High-level API for working with VC objects.

;;; Code:
(in-package :vc)
(defparameter *default-vc-kind* :hg)
(defvar *repo-roots* nil)
(defvar *repo-registry* (make-hash-table :test 'equal))

(defun register-repo (repo)
  "Register a repo, collecting information from the filesystem and
creating a repo object which is stored in *REPO-REGISTRY*."
  (setf (gethash (vc/proto:vc-path repo) *repo-registry*) repo))

(defun find-repo (name)
  "Find a repo in *REPO-REGISTRY*."
  (gethash name *repo-registry*))

(defun make-hg-repo (path &key init update register)
  (let ((repo (make-instance 'hg-repo :path path)))
    (when init (vc-init repo))
    (when update
      (setf (vc/hg::vc-requires repo)
            (mapcar (lambda (s) (trim s))
                    (sb-unicode:lines (vc-run repo "debugrequires")))))
    (when register (register-repo repo))
    repo))

(defun make-git-repo (path &key init register)
  (let ((repo (make-instance 'git-repo :path path)))
    (when init (vc-init repo))
    (when register (register-repo repo))
    repo))

(defun make-repo (path &key (type *default-vc-kind*) init register)
  (case type
    (:hg (make-hg-repo path :init init :register register))
    (:git (make-git-repo path :init init :register register))
    (t (error "invalid repo type: ~A" type))))

;; (defmacro with-hg ((&rest vc-opts) &body body))

;; (defmacro with-git ((&rest vc-opts) &body body))

;; (defmacro with-repos ((&rest repo-defs) &body body))

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
