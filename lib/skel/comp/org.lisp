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

(defmethods sk-load-component 
  (((kind (eql :org)) (form string) &optional (path (sk-src *skel-project*)))
   (sk-load-component kind (pathname form) path))
  (((kind (eql :org)) (form pathname) &optional (path (sk-src *skel-project*)))
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
