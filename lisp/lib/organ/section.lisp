;;; lib/organ/section.lisp --- Org Sections

;;

;;; Code:
(in-package :organ)

(defclass org-section () 
  ((contents :initform #() :initarg :contents :type (vector org-object))))

(defclass org-zeroth-section (org-section) ())
