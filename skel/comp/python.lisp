;;; python.lisp --- Python Components

;; ref: https://peps.python.org/pep-0621/

;;; Code:
(in-package :skel/comp/python)

(defparameter *pyproject-filename* "pyproject.toml")

(defcomponent python-system (project-module)
  (config))

(defcomponent python-component (project-component)
  (type value))

(defmethod print-object ((object python-system) stream)
  (print-unreadable-object (object stream :type t)
    (format stream ":ID ~A" (format-sxhash (obj/id:id object)))))

(defmethod load-project-component ((kind (eql :python-system)) (form pathname) &key (path (project-root)))
  (declare (ignore kind))
  (make-instance 'python-system :config (deserialize (merge-pathnames form path) :toml)))

(defmethod project-compile ((self python-system) &key &allow-other-keys))

(defmethod write-ast ((self python-system) &key path)
  (declare (ignorable path)))

(defmethod read-ast ((self python-system) path))
