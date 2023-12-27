;;; lib/organ/element/lesser/planning.lisp --- Org Planning Elements

;; A planning element matches the pattern:

#|
HEADING
PLANNING
|#

;; HEADING is just a heading. 

;; PLANNING matches the pattern: 'KEYWORD: TIMESTAMP'.

;; KEYWORD is one of DEADLINE, SCHEDULED, CLOSED.

;;; Code:
(in-package :organ)

;; helper object, not public API
(define-org-object planning-line (keyword timestamp))

(define-org-element planning ((lines :type (vector org-planning-line))) :lesser t)
