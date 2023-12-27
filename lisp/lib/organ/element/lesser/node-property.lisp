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

(define-org-element node-property (name value) :lesser t)
