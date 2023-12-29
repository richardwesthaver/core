;;; lib/organ/obj.lisp --- Org Heading

;;

;;; Code:
(in-package :organ)
(defun planning-line-p (l) (scan org-planning-rx l))
(defun property-start-p (l) (scan org-property-start-rx l))

(defun org-parse-planning-and-properties (input)
  "Parse INPUT returning the following values:

(PLANNING PROPERTIES REST)"
  (let ((planning) properties)
    (loop for l = (peek-line input)
          until (not l)
          when (is-planning-line l)
            do (push (org-parse :planning-line (read-line input)) planning)
          when (is-property-start l)
            do (setq properties (org-parse :property-drawer input)))
    (values planning properties (read-until-end input))))
  
(defclass org-heading () 
  ((headline :initarg :headline :initform (org-create :headline) :type org-headline :accessor org-headline)
   (planning :initarg :planning :initform nil :type (or null org-planning) :accessor org-planning)
   (properties :initarg :properties :initform nil :type (or null org-property-drawer) :accessor org-properties)
   (contents :initarg :contents :initform nil :type (or null (vector (or org-section org-heading))) 
             :accessor org-contents)))

(defmethod org-create ((type (eql :header)) &rest initargs &key &allow-other-keys)
  (apply #'make-instance (sym-to-org-class-name type) initargs))

(define-org-parser (heading :from stream)
    (let ((headline (org-parse :headline (read-line input))))
      (multiple-value-bind (planning properties rest)
          (org-parse-planning-and-properties input)
        (make-instance 'org-heading 
          :headline headline 
          :planning planning
          :properties properties
          :contents (org-parse :section (read-until-end rest))))))

(define-org-parser (heading :from string)
  (with-input-from-string (s input)
    (org-parse :heading s)))
