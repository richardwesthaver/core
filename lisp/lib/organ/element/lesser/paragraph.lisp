;;; lib/organ/element/paragraph.lisp --- Org Paragraph

;;

;;; Code:
(in-package :organ)

(define-org-element paragraph ((contents :initarg :contents :type (or list string))) :lesser t)

(define-org-parser (paragraph :from string)
  (with-slots (contents) paragraph
      (setf contents input)
    paragraph))
