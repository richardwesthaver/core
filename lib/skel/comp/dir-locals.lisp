;;; skel/comp/dir-locals.lisp --- Support for Emacs dir-locals.el

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Local-Variables.html

;;; Code:
(in-package :skel/comp/dir-locals)
(defvar *dir-locals-file* ".dir-locals.el")
(deftype dir-local-var-designator () '(or symbol string))

(defclass sk-dir-locals (sk-component ast)
  ())

(defmethod sk-new ((self (eql :dir-locals)) &rest args)
  (apply #'make-instance 'sk-dir-locals args))

(defmethod sk-load-component ((kind (eql :dir-locals)) (form pathname) &optional (path (project-root)))
  (declare (ignore kind))
  (sk-new :dir-locals :ast (read-sxp-file (make-pathname :name (namestring form) :type "el"
                                                         :directory (namestring path)))))
