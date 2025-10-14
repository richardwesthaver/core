;;; lib/organ/document.lisp --- Org Document API

;; Top-level file object

;;; Commentary:

;; ORG-DOCUMENT is the top-level Lisp representation of a complete Org-mode
;; file.

;;; Code:
(in-package :organ)

(defclass org-document (ast)
  ((meta :initform nil :initarg :meta :type (or null org-meta-section) :accessor doc-meta)))

(defmethod org-title ((self org-document))
  (org-title (doc-meta self)))

(defmethod org-create ((type (eql :document)) &rest initargs)
  (apply #'make-instance (sym-to-org-class-name type) initargs))

(defmethod org-parse ((type (eql :document)) (input stream))
  (let ((res (org-create type)))
    (setf (doc-meta res) (org-parse :meta input)
          (ast res)
          (coerce
           (loop for c = (peek-char nil input nil nil)
                 while (and c (char= c #\*))
                 collect (org-parse :heading input))
           '(vector org-heading)))
    res))

(defmethod org-parse ((type (eql :document)) (input pathname))
  (if (probe-file input)
      (with-open-file (fstream input)
        (org-parse :document fstream))
      (org-file-missing input)))

(defmethod org-parse ((type (eql :document)) (input string))
  (with-input-from-string (s input)
    (org-parse :document s)))
