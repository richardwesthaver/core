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

(defun load-user-skelrc (&optional (file *user-skelrc*) (init t))
  "Load a user-skelrc configuration from FILE. Defaults to *USER-SKELRC*.

If FILE does not exists, it is created with a default configuration."
  (flet ((%load () 
           (setq *skel-user-config* 
                 (load-ast 
                  (make-instance 'sk-user-config
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
          (load-ast (make-instance 'sk-system-config :ast #1=(file-read-forms f) :id (sxhash #1#) :path f)))
    (if auto
        (init-system-skelrc)
        *skel-system-config*)))

(eval-always
  (defun load-skelfile (file)
    "Load the 'skelfile' FILE."
    (load-ast (sk-read-file (make-instance 'sk-project) file)))

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

  (defun init-skelfile (&optional file name config)
    "Initialize a skelfile."
    (let ((sk (make-instance 'sk-project 
		:name (or name (pathname-name (sb-posix:getcwd)))))
	  (path (or file *default-skelfile*)))
      (when config (setf sk (wrap sk config)))
      (sk-write-file sk :path path :pretty t))))

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
  (ed (user-skelrc)))

(defun edit-system-skelrc ()
  "Open the current system configuration using ED."
  (ed *system-skelrc*))

(defun sk-config-slot (slot &optional (default :error))
  "First check *SKEL-USER-CONFIG* for a slot value, and if a valid value
isn't found check *SKEL-SYSTEM-CONFIG*."
  (let ((slot (find-symbol (string-upcase (string slot)) :skel/core)))
    (if (or (null *skel-user-config*) (not (slot-boundp* *skel-user-config* slot)))
        (if (or (null *skel-system-config*) (not (slot-boundp* *skel-system-config* slot)))
            (if (eql default :error)
                (skel-simple-error "slot is unbound in skelrc")
                default)
            (slot-value *skel-system-config* slot))
        (slot-value *skel-user-config* slot))))

(defun sk-project-slot (slot &optional (default :error))
  (let ((slot (find-symbol (string-upcase (string slot)) :skel/core)))
    (if (or (null *skel-project*) (not (slot-boundp* *skel-project* slot)))
        ;; Not found in project, search config files instead
        (sk-config-slot slot default)
        (slot-value *skel-project* slot))))

(defun sk-search-project (query &optional (project *skel-project*) 
                                          (user-config *skel-user-config*)
                                          (system-config *skel-system-config*))
  "Search the current project for elements matching QUERY."
  (etypecase query
    (string (or (sk-find query project)
                (sk-find query user-config)
                (when system-config
                  (sk-find query system-config))))
    (integer (or (sk-find query project :slot :id)
                 (sk-find query user-config :slot :id)
                 (when system-config
                   (sk-find query system-config :slot :id))))
    (keyword (sk-project-slot query))))

(macrolet 
    ((%init (set)
       `(with-readtable :shell
          (load-skelrc)
          (when-let ((cache (sk-config-slot :cache nil)))
            (,set *skel-cache* (ensure-directory-truename cache)))
          (when-let ((store (sk-config-slot :store nil)))
            (,set *skel-store* (ensure-directory-truename store)))
          (when-let ((stash (sk-config-slot :stash nil)))
            (,set *skel-stash* (ensure-directory-truename stash)))
          (when-let ((project (find-skelfile *default-pathname-defaults*)))
            (,set *skel-project* (load-skelfile project)
                  *skel-path* (skel/core::src *skel-project*)
                  *skel-cache* (skel/core::cache *skel-project*)))
          (when-let ((hook *skel-hook*))
            (funcall hook :init))
          (values))))
  (defun init-skel ()
    "Initialize the global SKEL environment:

*SKEL-SYSTEM-CONFIG*
*SKEL-USER-CONFIG*
*SKEL-PROJECT*
*SKEL-CACHE*
*SKEL-STORE*
*SKEL-STASH*
*SKEL-LOGGER*
ASDF:*USER-CACHE*"
    (init :xdg)
    (setq *user-skelrc* (user-skelrc))
    (%init setq))
  (defun setf-skel-vars () (%init setf)))

;; (defmacro sk-apply-path-relevancy (path &optional (context *default-pathname-defaults*)))
(defmethod init ((self (eql :skel)) &key) (init-skel))

(defun project-root (&optional (project *skel-project*))
  (or (when project (skel/core::src project)) *default-pathname-defaults*))

(defun merge-project-pathnames (path &optional (project *skel-project*))
  (merge-pathnames path (project-root project)))

(defun sk-project-clean (&optional (project *skel-project*))
  "Default function called to clean a SK-PROJECT."
  (with-directory (project-root project)
    (vc:vc-purge (vc:vc project))
    (when-let ((stash (probe-directory ".stash/")))
      (delete-directory stash :recursive t))))

(setq *default-clean-function* 'sk-project-clean)
