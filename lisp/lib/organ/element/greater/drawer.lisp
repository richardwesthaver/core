;;; lib/organ/element/greater/drawer.lisp --- Org Drawer Elements

;; Drawers match the pattern:

#|
:NAME:
CONTENTS
:end:
|#

;; Drawers can't be nested.

;;; Code:
(in-package :organ)

(define-org-element drawer (name contents) :greater t)

(define-org-element property-drawer 
    ((contents :initform (make-array 0 :element-type 'org-node-property :adjustable t :fill-pointer 0)
               :accessor org-contents :type (vector 'org-node-property)))
  :greater t
  :documentation "A special type of ORG-DRAWER with a names of
  'PROPERTIES'. This class is built into the slot of ORG-HEADING,
  ORG-DOCUMENT, and ORG-INLINETASK objects.")

(define-org-parser (property-drawer :from stream)
  (let ((l (read-line input nil :eof)))
    (unless (or (eq l :eof) (not (typep l 'string)))
      (if (scan org-property-start-rx l)
          (let ((drawer (org-create :property-drawer)))
            (loop for p = (read-line input nil :eof)
                  until (or (eq p :eof) (scan org-end-rx p))
                  do (vector-push-extend (org-parse :node-property p) (org-contents drawer)))
            drawer)))))

(define-org-parser (property-drawer :from string)
  (with-input-from-string (s input)
    (org-parse :property-drawer s)))
