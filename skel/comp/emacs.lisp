;;; skel/comp/emacs.lisp --- Support for Emacs components

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

(defmethod print-object ((object sk-emacs-component) stream)
  (print-unreadable-object (object stream :type t)
    (format stream ":ID ~A" (format-sxhash (id object)))))

;;; Org
(defclass sk-org-file (sk-component org-document sk-meta) ())

(defmethod sk-new ((self (eql :org)) &rest args)
  (apply #'make-instance 'sk-org-file args))

(defmethod sk-convert ((self org-document))
  (let ((self (change-class self 'sk-org-file)))
    (update-id self)
    self))

(defmethods sk-load-component 
  (((kind (eql :org)) (form string) &optional (path (project-root)))
   (sk-load-component kind (pathname form) path))
  (((kind (eql :org)) (form pathname) &optional (path (project-root)))
   (declare (ignore kind))
   (let* ((name (namestring form))
          (p (make-pathname :name name :type "org" :directory (namestring path)))
          (comp (sk-convert (org-parse :document p))))
     (setf (name comp) name)
     (setf (path comp) p)
     comp)))

(defmethod sk-compile ((self sk-org-file) &key)
  nil)

;; (describe (sk-load-component :org #p"readme"))
