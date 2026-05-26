;;; skel/comp/dir-locals.lisp --- Support for Emacs components

;; Emacs Lisp Components

;;; Commentary:

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Local-Variables.html

;;; Code:
(in-package :skel/comp/emacs)

(defvar *dir-locals-file* ".dir-locals.el")
(deftype dir-local-var-designator () '(or symbol string))


(defclass sk-emacs-component (sk-component ast)
  ())

(defclass sk-dir-locals (sk-emacs-component)
  ())

(defmethod sk-new ((self (eql :dir-locals)) &rest args)
  (apply #'make-instance 'sk-dir-locals args))

(defmethod sk-load-component ((kind (eql :dir-locals)) (form pathname) &optional (path (project-root)))
  (declare (ignore kind))
  (sk-new :dir-locals :ast (file-read-forms (make-pathname :name (namestring form) :type "el"
                                                           :directory (namestring path)))))

(defclass sk-emacs-lisp-file (sk-emacs-component)
  ())

(defmethod sk-new ((self (eql :el)) &rest args)
  (apply #'make-instance 'sk-emacs-lisp-file args))

(defmethod sk-load-component ((kind (eql :el)) (form pathname) &optional (path (project-root)))
  (declare (ignore kind))
  (sk-new :el 
          :ast (file-read-forms (make-pathname :name (namestring form) :type "el"
                                               :directory (namestring path)))))
