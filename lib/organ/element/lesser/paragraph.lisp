;;; lib/organ/element/paragraph.lisp --- Org Paragraph

;;

;;; Code:
(in-package :organ)

(define-org-element paragraph 
    ((contents :initarg :contents :type vector :accessor org-contents))
  :lesser t)

(define-org-parser (paragraph :from string)
  (let ((res (org-create :paragraph)))
    (setf (org-contents res) input)
    res))
