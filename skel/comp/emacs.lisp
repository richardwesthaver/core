;;; skel/comp/emacs.lisp --- Support for Emacs components

;; Emacs Lisp Components

;;; Commentary:

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Local-Variables.html

;;; Code:
(in-package :skel/comp/emacs)

(defvar *dir-locals-file* ".dir-locals.el")
(deftype dir-local-var-designator () '(or symbol string))

(defclass emacs-component (project-component) ())

(defclass dir-locals (emacs-component) ())

(defmethod load-project-component ((kind (eql :dir-locals)) (form pathname) &key (path (project-root)))
  (let ((self
          (make-instance 'dir-locals 
            :ast (file-read-forms (make-pathname :name (namestring form) :type "el"
                                                 :directory (namestring path))))))
    (update-id self)
    self))

(defmethod load-ast ((self emacs-component)) nil)

(defclass emacs-lisp-file (emacs-component) ())

(defmethod load-project-component ((kind (eql :el)) (form pathname) &key (path (project-root)))
  (let ((self
          (make-instance 'emacs-lisp-file
            :ast (file-read-forms (make-pathname :name (namestring form) :type "el"
                                                 :directory (namestring path))))))
    (update-id self)
    self))

(defmethod print-object ((object emacs-component) stream)
  (print-unreadable-object (object stream :type t)
    (format stream ":ID ~A" (format-sxhash (id object)))))

;;; Org
(defclass project-org-file (project-component org-document project-metadata) ())

(defmethod project-convert ((self org-document))
  (let ((self (change-class self 'project-org-file)))
    (update-id self)
    self))

(defmethods load-project-component 
  (((kind (eql :org)) (form string) &key (path (project-root)))
   (load-project-component kind (pathname form) :path path))
  (((kind (eql :org)) (form pathname) &key (path (project-root)))
   (declare (ignore kind))
   (let* ((name (namestring form))
          (p (make-pathname :name name :type "org" :directory (namestring path)))
          (comp (project-convert (org-parse :document p))))
     (setf (name comp) name)
     (setf (path comp) p)
     comp)))

(defmethod project-compile ((self project-org-file) &key)
  nil)

;; (describe (project-load-component :org #p"readme"))
