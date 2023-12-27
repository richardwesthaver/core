;;; lib/organ/object/link.lisp --- Org Link Objects

;; Org links come in the following subtypes: 

;; radio : PRE RADIO POST

;; plain : PRE PROTOCOL:PATHPLAIN POST

;; angle : <PROTOCOL:PATHANGLE>

;; regular : [[PATHREG]] or [[PATHREG][DESCRIPTION]]

;;; Code:
(in-package :organ)

(define-org-object link ())

(define-org-parser (link :from string)
  )
