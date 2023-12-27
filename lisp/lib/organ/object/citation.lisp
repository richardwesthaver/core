;; This file covers citations and citation references.

;; Citations match the following patterns:

#|
[cite CITESTYLE: REFERENCES]
[cite CITESTYLE: GLOBALPREFIX;REFERENCES]
[cite CITESTYLE: REFERENCES;GLOBALSUFFIX]
[cite CITESTYLE: GLOBALPREFIX;REFERENCES;GLOBALSUFFIX]
|#

;; Examples:

#|
[cite:@key]
[cite/t: see;@source1;@source2;by Smith /et al./]
|#
;; Citation references match the following patterns:

;; KEYPREFIX @KEY KEYSUFFIX

;;; Code:
(in-package :organ)

(define-org-object citation (style pfx refs sfx))

(define-org-object citation-reference (pfx key sfx))
