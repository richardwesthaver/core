;;; lib/organ/document.lisp --- Org Document API

;; Top-level file object

;;; Commentary:

;; ORG-DOCUMENT is the top-level Lisp representation of a complete Org-mode
;; file.

;;; Code:
(in-package :organ)

(defclass org-document ()
  ((meta :initform nil :initarg :meta :type (or null org-zeroth-section) :accessor doc-meta)
   (tree :initform nil :initarg :tree :type (or (vector org-heading) null) :accessor doc-tree)))

(defaccessor ast ((self org-document)) (doc-tree self))

(defmethod org-create ((type (eql :document)) &rest initargs)
  (apply #'make-instance (sym-to-org-class-name type) initargs))

(defmethod org-parse ((type (eql :document)) (input pathname))
  (if (probe-file input)
      (let ((res (org-create type)))
        (with-open-file (fstream input)
          (setf (doc-meta res) (org-parse :meta fstream)
                (doc-tree res)
                (coerce
                 (loop for c = (peek-char nil fstream nil nil)
                       while (and c (char= c #\*))
                       collect (org-parse :heading fstream))
                 '(vector org-heading)))
          res))
      (org-file-missing input)))
