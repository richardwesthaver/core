;;; lib/organ/section.lisp --- Org Sections

;;

;;; Code:
(in-package :organ)

(defclass org-section () 
  ((contents :initform #() :initarg :contents :type (vector org-object)
             :accessor org-contents)))

(defmethod org-create ((type (eql :section)) &rest initargs)
  (apply #'make-instance (sym-to-org-class-name type) initargs))

(defmethod org-parse ((type (eql :section)) (input string))
  (unless (sequence:emptyp input)
    (org-create :section :contents input)))

(defclass org-zeroth-section (org-section) ())

(defmethod org-create ((type (eql :meta)) &rest initargs)
  (apply #'make-instance (sym-to-org-class-name type) initargs))

(defmethod org-parse ((type (eql :meta)) (input string))
  (unless (sequence:emptyp input)
    (org-create :meta :contents input)))
