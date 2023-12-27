;;; lib/organ/object/target.lisp --- Org Target Objects

;; This file covers org targets and radio-targets.

;; targets match the pattern <<TARGET>>

;; radio-targets match the pattern <<<CONTENTS>>>
(in-package :organ)

(define-org-object target (contents))

(define-org-object radio-target () :include target)
