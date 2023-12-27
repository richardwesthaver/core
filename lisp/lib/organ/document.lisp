;;; lib/organ/document.lisp --- Org Document API

;;

;;; Code:
(in-package :organ)

(defclass org-document ()
  ((meta :initform nil :initarg :meta :type (or null org-zeroth-section) :accessor doc-meta)
   (tree :initform nil :initarg :tree :type (or (vector org-heading) null :accessor doc-tree))))

(defmethod org-create ((type (eql :document)) &rest initargs)
  (apply #'make-instance (kw->class type) initargs))

(defmethod org-parse ((type (eql :document)) (input pathname))
  (let ((res (org-create type)))
    (unless (probe-file input) (org-file-missing input))
    (with-open-file (fstream input)
      (setf (doc-meta res) (org-parse fstream)))
    res))
