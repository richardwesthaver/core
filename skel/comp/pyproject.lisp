;;; pyproject.lisp --- Pyproject.toml Support

;; ref: https://peps.python.org/pep-0621/

;;; Code:
(in-package :skel/comp/pyproject)

(defparameter *pyproject-filename* "pyproject.toml")

(defclass sk-python-system (sk-mod)
  ((config :initarg :config)))

(defclass sk-python-component (sk-component)
  (type value))

(defmethod print-object ((object sk-python-system) stream)
  (print-unreadable-object (object stream :type t)
    (format stream ":ID ~A" (format-sxhash (obj/id:id object)))))

(defmethod sk-load-component ((kind (eql :python-system)) (form pathname) &optional (path (project-root)))
  (declare (ignore kind))
  (make-instance 'sk-python-system :config (deserialize (merge-pathnames form path) :toml)))

(defmethod sk-compile ((self sk-python-system) &key &allow-other-keys))

(defmethod sk-write-file ((self sk-python-system) &key path)
  (declare (ignorable path)))

(defmethod sk-read-file ((self sk-python-system) path))

