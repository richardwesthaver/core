;;; lib/dat/condition.lisp --- DAT Conditions

;; Conditions signaled from the DAT system

;;; Code:
(in-package :dat/proto)

(define-condition dat-error (std-error) ())
