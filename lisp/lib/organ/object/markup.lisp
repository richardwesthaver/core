;;; lib/organ/object/markup.lisp --- Org Markup

;; includes PLAIN-TEXT, BOLD, ITALIC, UNDERLINE, VERBATIM, CODE, and STRIKE-THROUGH.

;;; Code:
(in-package :organ)

;; Any string that doesn't match another object is considered a plain
;; text object. Whitespace MAY be collapsed within any plain-text
;; instance.
(defstruct org-plain-text ())

(defstruct org-bold)

(defstruct org-italic)

(defstruct org-underline)

(defstruct org-verbatim)

(defstruct org-code)

(defstruct org-strike-through)

