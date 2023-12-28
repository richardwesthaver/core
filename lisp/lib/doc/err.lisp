;;; lib/doc/err.lisp --- Doc Errors

;; Error conditions for documentation

;; We build most of our docs offline so we can be a bit more noisy
;; with our handling.

;;; Code:
(in-package :doc)

(define-condition doc-error (error) ())
