;;; Utils
(in-package :skel/core)

(defun init-skelrc (&optional file)
  "Initialize a skelrc configuration based on the currently active
defaults. Defaults to ~/.skelrc."
  (sk-write-file *skel-user-config* :path (or file *default-skelrc*) :fmt :collapsed))

(defun load-skelrc (&optional file)
  "Load a skelrc configuration from FILE. Defaults to ~/.skelrc."
  (let ((form (file-read-forms (or file *default-user-skelrc*))))
    (load-ast (make-instance 'sk-user-config :ast form :id (sxhash form)))))

(defun init-skel-user-config (&optional file)
  "Initialize the *SKEL-USER-CONFIG* var."
  (setq *skel-user-config* (load-skelrc file)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun load-skelfile (file)
    "Load the 'skelfile' FILE."
    (load-ast (sk-read-file (make-instance 'sk-project) file)))

  (defun find-project-root (path name)
    "Return PATH if it is a `skel-project' by checking for
  NAME."
    (if (probe-file (merge-pathnames name path))
        path
        (let ((next (pathname-parent-directory-pathname path)))
	  (when (eql path next)
	    (find-project-root next name)))))

  (defun init-skelfile (&optional file name cfg)
    "Initialize a skelfile."
    (let ((sk (make-instance 'sk-project 
		:name (or name (pathname-name (sb-posix:getcwd)))))
	  (path (or file *default-skelfile*))
	  (fmt :collapsed))
      (when cfg (setf sk (sk-install-user-config sk cfg)))
      (sk-write-file sk :path path :fmt fmt))))

(defun find-skelfile (start &key (load nil) (name *default-skelfile*) (walk t))
  "Walk up the current directory returning the path to a 'skelfile', else
return nil. When LOAD is non-nil, load the skelfile if found."
  ;; Check the current path, if no skelfile found, walk up a level and
  ;; continue until the `*skelfile-boundary*' is triggered.
  (if walk 
      (let ((root (find-project-root (make-pathname :directory (pathname-directory start)) name)))
	(if root
	    (if load
		(load-skelfile (merge-pathnames name root))
		(merge-pathnames name root))
	    (warn "failed to find skelfile")))
      (if-let ((sk (probe-file (merge-pathnames name start))))
	(if load 
	    (load-skelfile sk)
	    sk)
	(warn "failed to find skelfile"))))

(defun describe-skeleton (skel &optional (stream t))
  "Describe the object SKEL which should inherit from the `skel' superclass."
  (print-object skel stream)
  (terpri stream))

(defun describe-project (&optional path (stream t))
  "Describe the project responsible for the pathname PATH. Defaults to
`sb-posix:getcwd'."
  (let* ((cd (or path (sb-posix:getcwd))))
    (print cd stream)
    (terpri stream)))

(defun edit-skelrc (&optional editor)
  "Open the current user configuration in EDITOR.")
