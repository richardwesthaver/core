;;; lib/organ/obj.lisp --- Org Heading

;;

;;; Code:
(in-package :organ)

(defclass org-heading ()
  ((headline :initarg :headline :initform (org-create :headline) :type org-headline :accessor org-headline)
   (planning :initarg :planning :initform nil :type (or null org-planning) :accessor org-planning)
   (properties :initarg :properties :initform nil :type (or null org-property-drawer) :accessor org-properties)
   (contents :initarg :contents :initform nil :type (or null (vector (or org-section org-heading))) 
             :accessor org-contents)))

(defmethod org-create ((type (eql :heading)) &rest initargs &key &allow-other-keys)
  (apply #'make-instance (sym-to-org-class-name type) initargs))

;; TODO 2024-03-17: fix org-parse-planning-properties -- hangs
(define-org-parser (heading :from stream)
  (when-let* ((l (read-line input))
              (headline (org-parse :headline l)))
    (let ((planning (org-parse :planning input)))
      (make-instance 'org-heading
        :headline headline
        :planning planning
        :properties (org-parse :property-drawer input)
        :contents (org-parse :section input)))))

(define-org-parser (heading :from string)
  (with-input-from-string (s input)
    (org-parse :heading s)))
