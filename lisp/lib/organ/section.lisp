;;; lib/organ/section.lisp --- Org Sections

;;

;;; Code:
(in-package :organ)

(defclass org-section () 
  ((contents :initform #() :initarg :contents :type (vector org-object)
             :accessor org-contents)))
;; TODO
(defmethod org-parse ((type (eql :section)) (input stream))
  (let ((res (org-create :section)))
    (setf (org-contents res) (with-output-to-string (s) s))))

(defclass org-zeroth-section (org-section) ())
;; TODO
(defmethod org-parse ((type (eql :meta)) (input stream))
  (let ((res (org-create :meta)))
    (setf (org-contents res) (with-output-to-string (s) s))))
