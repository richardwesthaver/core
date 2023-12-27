;;; lib/organ/section.lisp --- Org Sections

;;

;;; Code:
(in-package :organ)

(defclass org-section () 
  ((contents :initform #() :initarg :contents :type (vector org-object)
             :accessor org-contents)))

(defmethod org-parse ((type (eql :section)) (input stream))
  (let ((res (org-create :section)))
    (with-open-stream (in input)
      (setf (org-contents res) (with-output-to-string (s) s)))))

(defclass org-zeroth-section (org-section) ())

(defmethod org-parse ((type (eql :meta)) (input stream))
  (let ((res (org-create :meta)))
    (with-open-stream (in input)
      (setf (org-contents res) (with-output-to-string (s) s)))))
