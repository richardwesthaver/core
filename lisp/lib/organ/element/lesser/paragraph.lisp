;;; lib/organ/element/paragraph.lisp --- Org Paragraph

;;

;;; Code:
(in-package :organ)

(defclass org-paragraph (org-element) ())

(defmethod org-parse (input (type (eql :paragraph)))
  (%make-org type input))
