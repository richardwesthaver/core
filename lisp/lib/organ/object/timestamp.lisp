;;; lib/organ/object/timestamp.lisp --- Org Timestamp Object

;; Org timestamps come in one of the seven following patterns:
#|
<%%(SEXP)>                                                     (diary)
<DATE TIME REPEATER-OR-DELAY>                                  (active)
[DATE TIME REPEATER-OR-DELAY]                                  (inactive)
<DATE TIME REPEATER-OR-DELAY>--<DATE TIME REPEATER-OR-DELAY>   (active range)
<DATE TIME-TIME REPEATER-OR-DELAY>                             (active range)
[DATE TIME REPEATER-OR-DELAY]--[DATE TIME REPEATER-OR-DELAY]   (inactive range)
[DATE TIME-TIME REPEATER-OR-DELAY]                             (inactive range)
|#

;; As of 2023-12-26 we are ignoring the diary format. The remainder
;; are supported.

;; Timestamps can be of a unit kind (active/inactive above) or
;; represent a range. Active and Inactive timestamps are unrelated and
;; shouldn't inherit structure from each other. For example don't
;; define a slot named ACTIVE to distinguish them.

;;; Code:
(in-package :organ)

(define-org-object active-timestamp (date time mod))
(define-org-object active-timestamp-range (ts1 ts2))
(define-org-object inactive-timestamp (date time mod))
(define-org-object inactive-timestamp-range (ts1 ts2))
