;;; util.lisp --- VC High-level Utils

;; 

;;; Code:
(in-package :vc/util)

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
  `(with-directory (probe-directory ,path)
     (let ((,sym ,@(or (unless (keywordp (car args))
                         `(pop ,args))
                       `((make-repo 
                          *default-pathname-defaults* 
                          ,@(when init `(:init ,init)) ,@(when type `(:type ,type)))))))
       (setf *repo* ,sym)
       ,@body)))

;;; Clone
(defmethod vc-clone ((self pathname) (remote string) &key type)
  (let ((repo (if (or (search "git" remote)
                      (search "codeberg" remote)
                      (eql type :git))
                  (make-git-repo self)
                  (make-hg-repo self))))
    (vc-clone repo remote)))

(defmethod vc-clone ((self pathname) (remote uri) &key type)
  (vc-clone (pathname self) (uri-to-string remote) :type type))

(defmethod vc-clone ((self string) (remote t) &key type)
  (vc-clone (pathname self) remote :type type))

