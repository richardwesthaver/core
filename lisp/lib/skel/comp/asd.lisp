;;; lib/skel/comp/asd.lisp --- ASDF System Definition Compiler

;; ASDF/PARSE-DEFSYSTEM may come in handy for testing.

;; The problem with ASD files is that they're read-only afaik - eg there's no
;; 'write' methods implemented on ASD:SYSTEM objects. This makes it a bit
;; tedious because we obviously want to transform SK-LISP-SYSTEM objects
;; directly to SYSTEM, but also need to be able to write them out as discrete
;; files for portability. Probably will end up violating all that is DRY and
;; holy.

;;; Code:
(in-package :skel/comp/asd)

(defclass sk-lisp-system (skel asdf:system) ())

(defun read-system-definitions (system)
  (with-open-file (file (asdf:system-source-file system))
    (loop for x = (read file nil)
          while x
          collect x)))

(defun to-sk-system (system)
  (let ((sys (change-class system 'sk-lisp-system)))
    (id:update-id sys)
    sys))

(defun find-sk-system (system)
  (to-sk-system (asdf:find-system system)))

(defun parse-sk-system (name path &optional opts)
  (to-sk-system (asdf::parse-component-form nil (list* :system name :pathname path opts))))

(defmethod sk-load ((self sk-lisp-system) &key force force-not verbose version)
  (asdf:load-system self :force force :force-not force-not :verbose verbose :version version))

(defmethod sk-compile ((self sk-lisp-system) stream &key &allow-other-keys))

(defun sk-write-asd-components (module)
  (etypecase module
    (asdf:file-component
     (list (keywordicate (string-upcase (asdf:file-type module)))
           (pathname-name (asdf:component-relative-pathname module))))
    (asdf:module
     (list :module
           (asdf:component-name module)
           `(,@(when-let ((%c (asdf:module-components module)))
                 `((:components ,(mapcar #'sk-write-asd-components %c)))))))))

(defmethod sk-write-file ((self sk-lisp-system) &key path)
  (let ((name (asdf:component-name self)))
  (with-open-file (s path
                     :direction :output
                     :if-does-not-exist :create)
    (format s ";;; ASDF definition for system ~A~%" name)
            
    (format s ";;; Built for ~A ~A on a ~A/~A ~A~%"
            (lisp-implementation-type)
            (lisp-implementation-version)
            (software-type)
            (machine-type)
            (software-version))
    (let ((*package* (find-package :asdf-user))
          (*print-case* :downcase))
      (pprint `(defsystem ,name
                 :class prebuilt-system
                 :version ,(asdf:component-version self)
                 :depends-on ,(asdf:system-depends-on self)
                 :components ,(mapcar #'sk-write-asd-components
                                      (cdr (asdf:module-components self))))
              s)
      (terpri s)))))

;; (sk-write-file (find-sk-system :skel) :path "test")

(defmethod sk-read-file ((self sk-lisp-system) path)
  (parse-sk-system (pathname-name path) (pathname-directory path)))
