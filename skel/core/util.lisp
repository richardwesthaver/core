;;; Utils
(in-package :skel/core)
;;; Configs
(defun user-skelrc () (std:xdg-config-file :skel))
;; init-*,load-*
(defun load-skelrc (&optional (usr-path *user-skelrc*) (sys-path *system-skelrc*))
  (values
   (load-system-skelrc sys-path)
   (load-user-skelrc usr-path)))

(defun init-user-skelrc (&optional (file *user-skelrc*))
  "Initialize a skelrc configuration based on the currently active
*PROJECT-CONFIG*. Defaults to ~/.config/skelrc."
  (write-ast (make-instance 'skel-user-config) file :pretty t))

(defun init-system-skelrc (&optional (file *system-skelrc*))
  "Initialize a system skelrc configuration based on the currently active
*PROJECT-CONFIG*."
  (write-ast (make-instance 'skel-system-config) file :pretty t))

(defun load-user-skelrc (&optional (file *user-skelrc*) (init t))
  "Load a user-skelrc configuration from FILE. Defaults to *USER-SKELRC*.

If FILE does not exists, it is created with a default configuration."
  (flet ((%load () 
           (setq *skel-user-config* 
                 (load-ast 
                  (make-instance 'skel-user-config
                    :ast #1=(file-read-forms file)
                    :id (sxhash #1#)
                    :path file)))))
    (if (not init)
        (progn 
          (assert (probe-file file))
          (%load))
        (if (probe-file file)
            (%load)
            (init-user-skelrc file)))))

(defun load-system-skelrc (&optional (file *system-skelrc*) auto)
  "Load a skelrc configuration from FILE. Defaults to /etc/skel/skelrc.

Unlike LOAD-USER-SKELRC we don't generate a default file if one
doesn't exist, since it is assumed to be write-protected. This can be
overwritten with the AUTO flag."
  (if-let ((f (probe-file file)))
    (setq *skel-system-config*
          (load-ast (make-instance 'skel-system-config :ast #1=(file-read-forms f) :id (sxhash #1#) :path f)))
    (if auto
        (init-system-skelrc)
        *skel-system-config*)))

(eval-always
  (defun load-skelfile (file)
    "Load the 'skelfile' FILE."
    (load-ast (read-ast (make-instance 'skel-project) file)))

  (defun find-skel-project-root (path &optional (name *default-skelfile*))
    "Return the root location of a `skel-project' by checking for
  NAME."
      (if (probe-merge-file path name)
          path
          (let ((next (pathname-parent-directory-pathname path)))
            (unless (pathname-equal next path)
	      (find-skel-project-root next name)))))

  (defun find-skel-file (path ext)
    "Return the next SKEL-FILE at PATH matching the extension EXT."
    (if-let ((match (directory (merge-pathnames (format nil "*.~a" ext) path))))
      match
      (if-let ((match-root (directory (merge-pathnames *default-skelfile* path))))
        match-root
        (let ((next (pathname-parent-directory-pathname path)))
          (find-skel-file next ext)))))

  (defun init-skelfile (&optional file name config)
    "Initialize a skelfile."
    (let ((sk (make-instance 'skel-project 
		:name (or name (pathname-name (sb-posix:getcwd)))))
	  (path (or file *default-skelfile*)))
      (when config (setf sk (wrap sk config)))
      (write-ast sk path :pretty t))))

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
                   (if (pathname-equal next dir)
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
  (ed (list (namestring (user-skelrc)))))

(defun edit-system-skelrc ()
  "Open the current system configuration using ED."
  (ed (list (namestring *system-skelrc*))))

(macrolet 
    ((%init (set)
       `(with-readtable :shell
          (load-skelrc)
          (when-let ((cache (project-config-slot :cache :skel/core nil)))
            (,set *skel-cache* (ensure-directory-truename cache)))
          (when-let ((store (project-config-slot :store :skel/core nil)))
            (,set *skel-store* (ensure-directory-truename store)))
          (when-let ((stash (project-config-slot :stash :skel/core nil)))
            (,set *skel-stash* (ensure-directory-truename stash)))
          (when-let ((project (find-skelfile *default-pathname-defaults*)))
            (,set *project* (load-skelfile project)
                  *skel-path* (skel/core::src *project*)
                  *skel-cache* (skel/core::cache *project*)))
          (when-let ((hook *project-hook*))
            (funcall hook :init))
          (values))))
  (defun init-skel ()
    "Initialize the global SKEL environment:

*SKEL-SYSTEM-CONFIG*
*SKEL-USER-CONFIG*
*PROJECT*
*SKEL-CACHE*
*SKEL-STORE*
*SKEL-STASH*
*SKEL-LOGGER*
*USER-FASL-CACHE*"
    (init :xdg)
    (setq *user-skelrc* (user-skelrc))
    (%init setq)
    (setq *stash* *skel-stash*))
  (defun setf-skel-vars () (%init setf)))

;; (defmacro sk-apply-path-relevancy (path &optional (context *default-pathname-defaults*)))
(defmethod init ((self (eql :skel)) &key) (init-skel))

(defun project-root (&optional (project *project*))
  (or (when project (skel/core::src project)) *default-pathname-defaults*))

(defmethod load-project-component ((kind t) (form t) &key (path (project-root)))
  "Default component loader dispatches to DESERIALIZE."
  (let ((*default-pathname-defaults* path))
    (load-project-component kind form :path path)))

(defun merge-project-pathnames (path &optional (project *project*))
  (merge-pathnames path (project-root project)))

(defun skel-project-clean (&optional (project *project*))
  "Default function called to clean a SKEL-PROJECT."
  (with-directory (project-root project)
    (vc:vc-purge (vc:vc project))
    (when-let ((stash (probe-directory ".stash/")))
      (delete-directory stash :recursive t))))

(setq *default-clean-function* 'skel-project-clean)
