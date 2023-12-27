;;; lib/organ/element/paragraph.lisp --- Org Paragraph

;;

;;; Code:
(in-package :organ)

(define-org-element paragraph ((contents :initarg :contents :type (or list string) :accessor org-contents)) :lesser t)

(define-org-parser (paragraph :from string)
  (setf (org-contents paragraph) input)
  paragraph)
