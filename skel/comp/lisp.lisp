;;; lisp.lisp --- Lisp files

;; LISP-FILE

;;; Code:
(in-package :skel/comp/lisp)

(defclass lisp-component (project-component) ())

;;; Files
(defclass lisp-file (lisp-component file-component) ())

;; (defmethod sk-new ((self (eql :lisp)) &rest args)
;;   (apply #'make-instance 'sk-lisp-file args))

(defmethod project-convert ((self file-component))
  (make-instance 'lisp-file 
    :path (path self)
    :name (name self)
    :type "lisp"))

(defmethod project-compile ((self lisp-file) &rest args)
  (apply 'compile-file (path self) args))

(defmethod project-load ((self lisp-file) &key (compile t))
  (if compile
      (compile-and-load (path self))
      (load (path self))))

(defmethod exec ((self lisp-file))
  (compile-and-eval `(progn ,@(ast self))))
  
(defmethods load-project-component 
  (((self (eql :lisp)) (form pathname) &key (path (project-root)))
   (declare (ignore self))
   (let* ((type (pathname-type form))
          (name (namestring (if type (pathname-name form) form)))
          (fname (if type form (make-pathname :directory (namestring path) :name name :type "lisp")))
          (comp (make-instance 'lisp-file :parent *project* :path fname :name name)))
     comp))
  (((self (eql :lisp)) (form list) &key (path (project-root)))
   (let ((opts (cdr form))
         (comp (load-project-component self (pathname (car form)) :path (namestring path))))
     (when-let ((eval (getf opts :eval)))
       (case eval
         (:always (exec comp))
         ((or :never nil))
         (:load (project-load comp :compile nil))
         ;; default is :COMPILE
         (t (project-load comp :compile t))))
     (when (getf opts :read)
       (read-ast comp (path comp)))
     comp)))

;; (defmethod print-object ((object lisp-component) stream)
;;   (print-unreadable-object (object stream :type t)
;;     (format stream ":ID ~A" (format-sxhash (id object)))))

(defmethods read-ast 
  (((self lisp-component) stream) (setf (ast self) (read-lisp-until-end stream)) self)
  (((self lisp-component) (stream pathname)) (setf (ast self) (read-lisp-file stream)) self))

;; (defmethod sk-read-file ((self lisp-component) path)
;;   (with-input-from-file (f path)
;;     (read-ast self f)))

(defmethods write-ast 
  (((self lisp-component) stream &key) (write (ast self) :stream stream))
  (((self lisp-component) (stream pathname) &key) 
   (with-output-to-file (f stream)
     (write (ast self) :stream f))))

;; (defmethod sk-write-file ((self lisp-component) &key path)
;;   (with-output-to-file (f (or path (path self)))
;;     (write-ast self f)))

(defmethod load-ast ((self lisp-component))
  (if (ast self)
      (prog1 (exec self)
        (setf (ast self) nil))
      (project-load self)))

;;; System
(defclass lisp-system (lisp-component system) ())

(defun project-system-from-system (system)
  (let ((sys (change-class system 'lisp-system)))
    (id:update-id sys)
    sys))

(defmethod project-convert ((self system))
  (project-system-from-system self))

(defun find-lisp-system (system)
  (project-system-from-system (find-system system)))

(defun parse-lisp-system (name path &optional opts)
  (declare (ignore opts))
  (project-system-from-system (load-sys path name)))

(defmethod project-load ((self lisp-system) &key force verbose asdf)
  (load-system self :force force :verbose verbose :asdf asdf))

(std:defmethods load-project-component
  (((kind (eql :sys)) (form string) &key (path (project-root)))
   (load-project-component kind (pathname form) :path path))
  (((kind (eql :sys)) (form pathname) &key (path (project-root)))
   (declare (ignore kind))
   (let* ((type (pathname-type form))
          (name (namestring (if type (pathname-name form) form)))
          (fname (if type form (make-pathname :name name :type "sys"))))
     (parse-lisp-system name (merge-pathnames fname path)))))

(defmethod project-compile ((self lisp-system) &key force verbose asdf &allow-other-keys)
  (compile-system self :force force :verbose verbose :asdf asdf))

;;; ASDF
;; ASDF/PARSE-DEFSYSTEM may come in handy for testing.

;; The problem with ASD files is that they're read-only afaik - eg there's no
;; 'write' methods implemented on ASD:SYSTEM objects. This makes it a bit
;; tedious because we obviously want to transform ASDF-SYSTEM objects
;; directly to ASDF:SYSTEM, but also need to be able to write them out as
;; discrete files for portability. Probably will end up violating all that is
;; DRY and holy.

(defclass asdf-system (lisp-component asdf:system)
  ;; these slots are inferred in ASDF:SYSTEM. Since we are also concerned with
  ;; generating ASDF:SYSTEM definitions rather than just parsing them we
  ;; restore them here.
  ((serial :initform nil :type boolean :accessor asdf-system-serial)
   (perform :initform nil :type list :accessor asdf-system-perform)))

(defmethod name ((self asdf-system)) (asdf::coerce-name self))

(defun read-system-definitions (system)
  (with-open-file (file (asdf:system-source-file system))
    (loop for x = (read file nil)
          while x
          collect x)))

(defun project-system-from-asdf (system)
  (let ((sys (change-class system 'asdf-system)))
    (setf (asdf-system-serial sys) nil
          (asdf-system-perform sys) nil)
    (id:update-id sys)
    sys))

(defmethod project-convert ((self asdf:system))
  (project-system-from-asdf self))

(defun find-asdf-system (system)
  (project-system-from-asdf (asdf:find-system system)))

(defun parse-asdf-system (name path &optional opts)
  (project-system-from-asdf (asdf::parse-component-form nil (list* :system name :pathname path opts))))

(defmethod project-load ((self asdf-system) &key force force-not verbose version)
  (asdf:load-system self :force force :force-not force-not :verbose verbose :version version))

(defmethods load-project-component 
  (((kind (eql :asd)) (form string) &key (path (project-root)))
   (load-project-component kind (pathname form) :path path))
  (((kind (eql :asd)) (form pathname) &key (path (project-root)))
   (declare (ignore kind))
   (let* ((type (pathname-type form))
          (name (namestring (if type (pathname-name form) form)))
          (fname (if type form (make-pathname :name name :type "asd"))))
     (parse-asdf-system name (merge-pathnames fname path)))))

(defmethod project-compile ((self asdf-system) &key force force-not verbose version &allow-other-keys)
  (asdf:compile-system self :force force :force-not force-not :verbose verbose :version version))

(defun write-asd-components (module)
  (etypecase module
    (asdf:file-component
     `(,(std:keywordicate (string-upcase (asdf:file-type module)))
       ,(pathname-name (asdf:component-relative-pathname module))
       ,@(when-let ((x (asdf::component-if-feature module)))
           `(:if-feature ,x))
       ,@(when-let ((x (asdf::component-depends-on nil module)))
           `(:depends-on ,x))))
    (asdf:module
     `(:module
       ,(asdf:component-name module)
       ,@(when-let ((x (asdf::component-if-feature module)))
           `(:if-feature ,x))
       ,@(when-let ((x (asdf::component-depends-on nil module)))
           `(:depends-on ,x))
       ,@(when-let ((x (asdf:module-components module)))
           `(:components ,(mapcar #'write-asd-components x)))))))

(defmethod write-ast ((self asdf-system) (path pathname) &key)
  (let ((name (asdf:component-name self)))
    (with-open-file (s path
                       :direction :output
                       :if-does-not-exist :create)
      (format s ";;; ASDF definition for system ~A" name)
      (let ((*print-case* :downcase))
        (pprint `(defsystem ,name
                   :class asdf-system
                   ,@(when-let ((x (asdf:component-version self))) `(:version ,x))
                   ,@(when-let ((x (asdf:system-depends-on self))) `(:depends-on ,x))
                   ,@(when-let ((x (asdf:system-description self))) `(:description ,x))
                   ,@(when-let ((x (asdf:system-long-description self))) `(:long-description ,x))
                   ,@(when-let ((x (asdf:system-author self))) `(:author ,x))
                   ,@(when-let ((x (asdf:system-maintainer self))) `(:maintainer ,x))
                   ,@(when-let ((x (asdf:system-mailto self))) `(:mailto ,x))
                   ,@(when-let ((x (asdf::system-license self))) `(:license ,x))
                   ,@(when-let ((x (asdf:system-homepage self))) `(:homepage ,x))
                   ,@(when-let ((x (asdf:system-bug-tracker self))) `(:bug-tracker ,x))
                   ,@(when-let ((x (asdf:system-source-control self))) `(:source-control ,x))
                   ,@(when-let ((x (asdf::component-in-order-to self))) `(:in-order-to ,x))
                   ,@(when-let ((x (asdf::component-build-pathname self))) `(:build-pathname ,x))
                   ,@(when-let ((x (asdf::component-build-operation self))) `(:build-operation ,x))
                   ,@(when-let ((x (asdf::component-entry-point self))) `(:entry-point ,x))
                   ,@(when-let ((x (asdf-system-perform self))) `(:perform ,x))
                   ,@(when-let ((x (asdf-system-serial self))) `(:serial ,x))
                   :components ,(mapcar #'write-asd-components
                                        (asdf:module-components self)))
                s)
        (terpri s)))))

;; (write-file (find-asdf-system :obj) :path "test")
;; (describe (parse-asdf-system "skel" "/home/ellis/src/core/lib/"))

(defmethod read-ast ((self asdf-system) (path pathname))
  (parse-asdf-system (pathname-name path) (pathname-directory path)))
