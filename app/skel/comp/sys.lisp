;;; sys.lisp --- Core Lisp System Files

;; .sys

;;; Code:
(in-package :skel/comp/sys)

(defclass sk-lisp-system (sk-mod system) ())

(defun sys-to-sk-system (system)
  (let ((sys (change-class system 'sk-lisp-system)))
    (id:update-id sys)
    sys))

(defmethod sk-convert ((self system))
  (sys-to-sk-system self))

(defun find-sk-lisp-system (system)
  (sys-to-sk-system (find-system system)))

(defun parse-sk-lisp-system (name path &optional opts)
  (declare (ignore opts))
  (sys-to-sk-system (load-sys path name)))

(defmethod sk-load ((self sk-lisp-system) &key force verbose asdf)
  (load-system self :force force :verbose verbose :asdf asdf))

(std:defmethods sk-load-component
  (((kind (eql :sys)) (form string) &optional (path (project-root)))
   (sk-load-component kind (pathname form) path))
  (((kind (eql :sys)) (form pathname) &optional (path (project-root)))
   (declare (ignore kind))
   (let* ((type (pathname-type form))
          (name (namestring (if type (pathname-name form) form)))
          (fname (if type form (make-pathname :name name :type "sys"))))
     (parse-sk-lisp-system name (merge-pathnames fname path)))))

(defmethod sk-compile ((self sk-lisp-system) &key force verbose asdf &allow-other-keys)
  (compile-system self :force force :verbose verbose :asdf asdf))

