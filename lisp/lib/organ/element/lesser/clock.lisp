;;; lib/organ/element/lesser/clock.lisp --- Org Clock Element

;; Clock elements match the following:

#|
clock: INACTIVE-TIMESTAMP
clock: INACTIVE-TIMESTAMP-RANGE DURATION
|#

;; DURATION matches '=> HH:MM' where HH is +inf and MM is 2 digits
;; (realistically 0-59).

;;; Code:
(in-package :organ)

(define-org-element clock (timestamp duration) :lesser t)
