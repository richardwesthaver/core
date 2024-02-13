;;; Utils
(in-package :skel/core)

;;; Configs

;; init-*,load-*
(defun load-skelrc (&optional (usr *user-skelrc*) (sys *system-skelrc*))
  (values
   (load-system-skelrc sys)
   (load-user-skelrc usr)))

(defun init-user-skelrc (&optional (file *user-skelrc*))
  "Initialize a skelrc configuration based on the currently active
*SKEL-USER-CONFIG*. Defaults to ~/.skelrc."
  (sk-write-file *skel-user-config* 
                 :path file
                 :fmt :collapsed))

(defun init-system-skelrc (&optional (file *system-skelrc*))
  "Initialize a system skelrc configuration based on the currently active
*SKEL-SYSTEM-CONFIG*."
  (sk-write-file *skel-system-config*
                 :path file
                 :fmt :collapsed))

(defun load-user-skelrc (&optional (file *user-skelrc*))
  "Load a user-skelrc configuration from FILE. Defaults to *USER-SKELRC*.

If FILE does not exists, it is created with a default configuration."
  (if-let ((f (probe-file file)))
      (setq *skel-user-config* (load-ast 
                                (make-instance 'sk-user-config 
                                  :ast #1=(file-read-forms f) :id (sxhash #1#)
                                  :path f)))
      (init-user-skelrc)))

(defun load-system-skelrc (&optional (file *system-skelrc*) auto)
  "Load a skelrc configuration from FILE. Defaults to /etc/skel/skelrc.

Unlike LOAD-USER-SKELRC we don't generate a default file if one
doesn't exist, since it is assumed to be write-protected. This can be
overwritten with the AUTO flag."
  (if-let ((f (probe-file file)))
    (setq *skel-system-config*
          (load-ast (make-instance 'sk-system-config :ast #1=(file-read-forms f) :id (sxhash #1#) :path f)))
    (when auto (init-system-skelrc))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun load-skelfile (file)
    "Load the 'skelfile' FILE."
    (load-ast (sk-read-file (make-instance 'sk-project) file)))

  (defun find-sk-project-root (path name)
    "Return the root location of a `skel-project' by checking for
  NAME."
    (if (probe-file (merge-pathnames name path))
        path
        (let ((next (pathname-parent-directory-pathname path)))
	  (find-sk-project-root next name))))

  (defun find-sk-file (path ext)
    "Return the next SK-FILE at PATH matching the extension EXT."
    (if-let ((match (directory (merge-pathnames (format nil "*.~a" ext) path))))
      match
      (if-let ((match-root (directory (merge-pathnames *default-skelfile* path))))
        match-root
        (let ((next (pathname-parent-directory-pathname path)))
          (find-sk-file next ext)))))

  (defun init-skelfile (&optional file name cfg)
    "Initialize a skelfile."
    (let ((sk (make-instance 'sk-project 
		:name (or name (pathname-name (sb-posix:getcwd)))))
	  (path (or file *default-skelfile*))
	  (fmt :collapsed))
      (when cfg (setf sk (sk-install-user-config sk cfg)))
      (sk-write-file sk :path path :fmt fmt))))

(defun find-skelfile (start &key (load nil) (filename *default-skelfile*) (walk t))
  "Walk up the current directory returning the path to a 'skelfile', else
return nil. When LOAD is non-nil, load the skelfile if found."
  ;; Check the current path, if no skelfile found, walk up a level and
  ;; continue until the `*skelfile-boundary*' is triggered.
  (if walk 
      (let ((root (find-sk-project-root (make-pathname :directory (pathname-directory start)) filename)))
	(if root
	    (if load
		(load-skelfile (merge-pathnames filename root))
		(merge-pathnames filename root))
	    (warn "failed to find root skelfile")))
      (if-let ((sk (probe-file (merge-pathnames filename start))))
	(if load 
	    (load-skelfile sk)
	    sk)
	(warn "failed to find root skelfile"))))

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

(defun edit-skelrc ()
  "Open the current user configuration using ED."
  (ed *user-skelrc*))

(defun edit-system-skelrc ()
  "Open the current system configuration using ED."
  (ed *system-skelrc*))

(defun get-config-slot* (slot)
  "First check *SKEL-USER-CONFIG* for a slot value, and if a valid value
isn't found check *SKEL-SYSTEM-CONFIG*."
  (when (boundp '*skel-user-config*)
    (if (slot-unbound 'sk-user-config *skel-user-config* slot)
        (when (boundp '*skel-system-config*)
          (if (slot-unbound 'sk-system-config *skel-system-config* slot)
              (error 'skel-error :message (format nil "slot is unbound: ~a" slot))
              (slot-value *skel-system-config* slot)))
      (slot-value *skel-user-config* slot))))

(defun init-skel-vars ()
  "Initialize the global SKEL variables based on the active
*SKEL-USER-CONFIG*."
  (setq *skel-cache* (sk-cache *skel-user-config*)
        *skel-stash* (sk-stash *skel-user-config*)
        *skel-registry* (sk-registry *skel-user-config*)))
