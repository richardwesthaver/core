;;; lib/organ/obj.lisp --- Org Heading

;;

;;; Code:
(in-package :organ)

(defun org-parse-planning-and-properties (input)
  "Parse INPUT returning the following values:

(PLANNING PROPERTIES REST)")
  
(defclass org-heading () 
  ((headline :initarg :headline :initform (org-create :headline) :type org-headline :accessor org-headline)
   (planning :initarg :planning :initform nil :type (or null org-planning) :accessor org-planning)
   (properties :initarg :properties :initform nil :type (or null org-property-drawer) :accessor org-properties)
   (contents :initarg :contents :initform nil :type (or null (vector (or org-section org-heading))) 
             :accessor org-contents)))

(defmethod org-create ((type (eql :header)) &rest initargs &key &allow-other-keys)
  (apply #'make-instance (sym-to-org-class-name type) initargs))

(define-org-parser (heading :from stream)
    (let ((headline (org-parse :headline (read-line input)))
          (planning nil) properties)
      ;; planning and properties modify the input stream here
      ;; (setq planning (org-parse :planning input))
      (setq properties (org-parse :property-drawer input))
      (make-instance 'org-heading 
        :headline headline 
        :planning planning
        :properties properties
        :contents (org-parse :section (read-until-end input)))))

(define-org-parser (heading :from string)
  (with-input-from-string (s input)
    (org-parse :heading s)))
