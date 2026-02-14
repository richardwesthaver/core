;;; lib/skel/comp/asd.lisp --- ASDF Components

;; ASDF/PARSE-DEFSYSTEM may come in handy for testing.

;; The problem with ASD files is that they're read-only afaik - eg there's no
;; 'write' methods implemented on ASD:SYSTEM objects. This makes it a bit
;; tedious because we obviously want to transform SK-ASDF-SYSTEM objects
;; directly to ASDF:SYSTEM, but also need to be able to write them out as
;; discrete files for portability. Probably will end up violating all that is
;; DRY and holy.

;;; Code:
(in-package :skel/comp/asd)

(defclass sk-asdf-system (sk-mod asdf:system)
  ;; these slots are inferred in ASDF:SYSTEM. Since we are also concerned with
  ;; generating ASDF:SYSTEM definitions rather than just parsing them we
  ;; restore them here.
  ((serial :initform nil :type boolean :accessor sk-asdf-system-serial)
   (perform :initform nil :type list :accessor sk-asdf-system-perform)))

(defmethod name ((self sk-asdf-system)) (asdf::coerce-name self))

(defun read-system-definitions (system)
  (with-open-file (file (asdf:system-source-file system))
    (loop for x = (read file nil)
          while x
          collect x)))

(defun asd-to-sk-system (system)
  (let ((sys (change-class system 'sk-asdf-system)))
    (setf (sk-asdf-system-serial sys) nil
          (sk-asdf-system-perform sys) nil)
    (id:update-id sys)
    sys))

(defmethod sk-convert ((self asdf:system))
  (asd-to-sk-system self))

(defun find-sk-asdf-system (system)
  (asd-to-sk-system (asdf:find-system system)))

(defun parse-sk-asdf-system (name path &optional opts)
  (asd-to-sk-system (asdf::parse-component-form nil (list* :system name :pathname path opts))))

(defmethod sk-load ((self sk-asdf-system) &key force force-not verbose version)
  (asdf:load-system self :force force :force-not force-not :verbose verbose :version version))

(defmethods sk-load-component 
  (((kind (eql :asd)) (form string) &optional (path (project-root)))
   (sk-load-component kind (pathname form) path))
  (((kind (eql :asd)) (form pathname) &optional (path (project-root)))
   (declare (ignore kind))
   (let* ((type (pathname-type form))
          (name (namestring (if type (pathname-name form) form)))
          (fname (if type form (make-pathname :name name :type "asd"))))
     (parse-sk-asdf-system name (merge-pathnames fname path)))))

(defmethod sk-compile ((self sk-asdf-system) &key force force-not verbose version &allow-other-keys)
  (asdf:compile-system self :force force :force-not force-not :verbose verbose :version version))

(defun sk-write-asd-components (module)
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
           `(:components ,(mapcar #'sk-write-asd-components x)))))))

(defmethod sk-write-file ((self sk-asdf-system) &key path)
  (let ((name (asdf:component-name self)))
    (with-open-file (s path
                       :direction :output
                       :if-does-not-exist :create)
      (format s ";;; ASDF definition for system ~A" name)
      (let ((*print-case* :downcase))
        (pprint `(defsystem ,name
                   :class sk-asdf-system
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
                   ,@(when-let ((x (sk-asdf-system-perform self))) `(:perform ,x))
                   ,@(when-let ((x (sk-asdf-system-serial self))) `(:serial ,x))
                   :components ,(mapcar #'sk-write-asd-components
                                        (asdf:module-components self)))
                s)
        (terpri s)))))

;; (sk-write-file (find-sk-asdf-system :obj) :path "test")
;; (describe (parse-sk-asdf-system "skel" "/home/ellis/comp/core/lib/"))

(defmethod sk-read-file ((self sk-asdf-system) path)
  (parse-sk-asdf-system (pathname-name path) (pathname-directory path)))
