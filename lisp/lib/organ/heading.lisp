;;; lib/organ/obj.lisp --- Org Heading

;;

;;; Code:
(in-package :organ)

(defclass org-heading () 
  ((headline :initarg :headline :initform (org-create :headline) :type org-headline)
   (contents :initarg :contents :initform #() :type (vector (or org-section org-heading)))))
