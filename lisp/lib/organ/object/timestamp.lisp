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

(defvar org-duration-hmm-rx (create-scanner "\\`[ \t]*[0-9]+\\(?::[0-9]\\{2\\}\\)\\{1,2\\}[ \t]*\\'")
  "Regexp matching a duration expressed with H:MM or H:MM:SS format.
See `org-duration-hmmss-rx' to only match the latter.  Hours
can use any number of digits.")

(defvar org-duration-hmmss-rx (create-scanner "\\`[ \t]*[0-9]+\\(?::[0-9]\\{2\\}\\)\\{2\\}[ \t]*\\'")
  "Regexp matching a duration expressed H:MM:SS format.
See `org-duration-hmm-rx' to also support H:MM format.  Hours
can use any number of digits.")

(defvar org-duration-full-rx 
  (create-scanner "\\`\\(?:[ 	]*\\([0-9]+\\(?:\\.[0-9]*\\)?\\)[ 	]*\\(min\\|[dhmwy]\\)\\)+[ 	]*\\'")
  "Regexp matching a duration expressed with units.")

(defvar org-duration-mixed-rx
  (create-scanner "\\`\\(?1:\\([ 	]*\\([0-9]+\\(?:\\.[0-9]*\\)?\\)[ 	]*\\(min\\|[dhmwy]\\)\\)+\\)[ 	]*\\(?2:[0-9]+\\(?::[0-9][0-9]\\)\\{1,2\\}\\)[ 	]*\\'")
  "Regexp matching a duration with units and H:MM or H:MM:SS format.")

(defvar org-duration-units
  '(("min" . 1) ("h" . 60) ("d" . 1440) ("w" . 10080) ("m" . 43200)
    ("y" . 525960.0)))

(define-org-object active-timestamp (date time mod))
(define-org-object active-timestamp-range (ts1 ts2))
(define-org-object inactive-timestamp (date time mod))
(define-org-object inactive-timestamp-range (ts1 ts2))
