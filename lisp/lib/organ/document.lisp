;;; lib/organ/document.lisp --- Org Document API

;;

;;; Code:
(in-package :organ)

(defclass org-document ()
  ((meta :initform nil :initarg :meta :type (or null org-zeroth-section) :accessor doc-meta)
   (tree :initform nil :initarg :tree :type (or (vector org-heading) null :accessor doc-tree))))

(defmethod org-create ((type (eql :document)) &rest initargs)
  (apply #'make-instance (sym-to-org-class-name type) initargs))

(defmethod org-parse ((type (eql :document)) (input pathname))
  (if (probe-file input) 
      (let ((res (org-create type)))
        (with-open-file (fstream input)
          (setf (doc-meta res) (org-parse :meta fstream))))
      (org-file-missing input)))

