;;; Utils
(in-package :skel/core/util)

;;; Configs

;; init-*,load-*
(defun load-skelrc (&optional (usr-path *user-skelrc*) (sys-path *system-skelrc*))
  (values
   (load-system-skelrc sys-path)
   (load-user-skelrc usr-path)))

(defun init-user-skelrc (&optional (file *user-skelrc*))
  "Initialize a skelrc configuration based on the currently active
*SKEL-USER-CONFIG*. Defaults to ~/.skelrc."
  (sk-write-file (make-instance 'sk-user-config)
                 :path file
                 :pretty t))

(defun init-system-skelrc (&optional (file *system-skelrc*))
  "Initialize a system skelrc configuration based on the currently active
*SKEL-SYSTEM-CONFIG*."
  (sk-write-file (make-instance 'sk-system-config)
                 :path file
                 :pretty t))

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
    (if auto
        (init-system-skelrc)
        *skel-system-config*)))

(eval-always
  (defun load-skelfile (file)
    "Load the 'skelfile' FILE."
    (load-ast (sk-read-file (make-instance 'sk-project) file)))

  (defun probe-merge-file (name path)
    (probe-file (merge-pathnames name path)))

  (defun find-sk-project-root (path &optional (name *default-skelfile*))
    "Return the root location of a `skel-project' by checking for
  NAME."
      (if (probe-merge-file path name)
          path
          (let ((next (pathname-parent-directory-pathname path)))
            (unless (uiop:pathname-equal next path)
	      (find-sk-project-root next name)))))

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
	  (fmt :pretty))
      (when cfg (setf sk (sk-install-user-config sk cfg)))
      (sk-write-file sk :path path :fmt fmt))))

(defun find-skelfile (start &key (load nil) (name *default-skelfile*) (ext "sk") (walk t) error)
  "Walk up the current directory returning the path to a 'skelfile' by NAME or a
filename with extension EXT, else return nil. When LOAD is non-nil, load the
skelfile if found."
  ;; Check the current path, if no skelfile found, walk up a level and
  ;; continue until the `*skelfile-boundary*' is triggered.
  (labels ((%check (dir)
             (or (probe-merge-file name dir)
                 (when-let ((match (directory (merge-pathnames dir (format nil "*.~a" ext)))))
                   (probe-file (car match)))
                 (probe-merge-file (make-pathname :name name :type ext) dir)))
           (%walk (dir)
             (or (%check dir)
                 (let ((next (pathname-parent-directory-pathname dir)))
                   (if (uiop:pathname-equal next dir)
                       (when error (skel-simple-error "failed to find root skelfile"))
                       (%walk next)))))
           (%load? (file) (if load (load-skelfile file) file)))
    (setf start (car (directory start)))
    (if-let ((match (%check start)))
      (%load? match)
      (if walk
          (when-let ((match (%walk start)))
            (%load? match))
	  (when error (skel-simple-error "failed to find root skelfile"))))))

(defun edit-skelrc ()
  "Open the current user configuration using ED."
  (ed *user-skelrc*))

(defun edit-system-skelrc ()
  "Open the current system configuration using ED."
  (ed *system-skelrc*))

(defun get-skelrc-slot* (slot &optional (default (skel-simple-error "slot is unbound in skel config")))
  "First check *SKEL-USER-CONFIG* for a slot value, and if a valid value
isn't found check *SKEL-SYSTEM-CONFIG*."
  (let ((slot (find-symbol (string-upcase (string slot)) :skel/core/obj)))
    (if (or (null *skel-user-config*) (not (slot-boundp *skel-user-config* slot)))
        (if (or (null *skel-system-config*) (not (slot-boundp *skel-system-config* slot)))
            default
            (slot-value *skel-system-config* slot))
        (slot-value *skel-user-config* slot))))

(defun init-skel-vars ()
  "Initialize the global SKEL variables:

*SKEL-SYSTEM-CONFIG*
*SKEL-USER-CONFIG*
*SKEL-PROJECT*
*SKEL-CACHE*
*SKEL-STORE*
*SKEL-STASH*
*SKEL-REGISTRY*"
  (in-readtable :shell)
  (load-skelrc)
  (when-let ((project (find-skelfile *default-pathname-defaults*)))
    (setq *skel-project* (load-skelfile project)
          *skel-path* #1=(sk-src *skel-project*)
          cli/shell:*shell-directory* #1#))
  (when-let ((cache (get-skelrc-slot* :cache nil)))
    (setq *skel-cache* cache))
  (when-let ((store (get-skelrc-slot* :store nil)))
    (setq *skel-store* store))
  (when-let ((stash (get-skelrc-slot* :stash nil)))
    (setq *skel-stash* stash))
  (when-let ((registry (get-skelrc-slot* :registry nil)))
    (setq *skel-registry* registry))
  (values))

;;; Paths

(defun parse-sk-path (input &optional (start 0) end)
  "An 'sk-path' is a CLI argument which can be translated into a corresponding
nested object."
  (with-input-from-string (s input)
    (loop for i from start below end
          collect (read-char s))))

;; (defmacro sk-apply-path-relevancy (path &optional (context *default-pathname-defaults*)))

(defun find-sk-path (path &optional skel)
  "Find an sk-path string in a skel object, or attempt to match it against all
active objects.")
