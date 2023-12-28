;;; lib/organ/element/lesser/node-property.lisp --- Org Node Property Element

;; Node properties match the patterns:

#|
:NAME: VALUE
:NAME:
:NAME+: VALUE
:NAME+:
|#

;;; Code:
(in-package :organ)

(define-org-element node-property ((name :initarg :name) (value :initarg :value)) :lesser t)

;; assumes caller is org-property-drawer = single line
(define-org-parser (node-property :from string)
  (multiple-value-bind (match subs)
      (scan-to-strings org-property-rx input)
    (when match
      (let ((name (aref subs 0))
            (value (aref subs 1)))
        (org-create :node-property :name name :value value)))))
