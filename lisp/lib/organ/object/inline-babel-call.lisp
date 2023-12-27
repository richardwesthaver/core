;;; lib/organ/object/inline-babel-call.lisp --- Org Inline Babel Call Object

;; Inline babel calls match the following patterns:

#|
call_NAME(ARGUMENTS)
call_NAME[HEADER1](ARGUMENTS)
call_NAME(ARGUMENTS)[HEADER2]
call_NAME[HEADER1](ARGUMENTS)[HEADER2]
|#

;;; Code:
(in-package :organ)

(define-org-object inline-babel-call (name header1 arguments header2))
