;;; org.lisp --- Skel Org File Components

;; 

;;; Code:
(in-package :skel/comp/org)

(defclass sk-org-file (sk-component org-document sk-meta) ())

(defmethod sk-new ((self (eql :org)) &rest args)
  (apply #'make-instance 'sk-org-file args))

(defmethod sk-convert ((self org-document))
  (let ((self (change-class self 'sk-org-file)))
    (update-id self)
    self))

(defmethod sk-load-component ((kind (eql :org)) (name pathname))
  (declare (ignore kind))
  (let* ((name (namestring name))
         (path (make-pathname :name name :type "org"))
         (comp (sk-convert (org-parse :document path))))
    (setf (sk-name comp) name)
    (setf (sk-path comp) path)
    comp))

;; (describe (sk-load-component :org #p"readme"))



