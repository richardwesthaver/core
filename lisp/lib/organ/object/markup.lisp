;;; lib/organ/object/markup.lisp --- Org Markup

;; includes PLAIN-TEXT, BOLD, ITALIC, UNDERLINE, VERBATIM, CODE, and STRIKE-THROUGH.

;;; Code:
(in-package :organ)

;; Any string that doesn't match another object is considered a plain
;; text object. Whitespace MAY be collapsed within any plain-text
;; instance.
(define-org-object plain-text (contents))

(define-org-parser (plain-text :from string)
  (setf (org-plain-text-contents plain-text) input))

;; *bold*
(define-org-object bold () :include plain-text)

(define-org-parser (bold :from string)
  (setf (org-bold-contents bold) (string-trim '(#\*) input)))

;; /italic/
(define-org-object italic () :include plain-text)

(define-org-parser (italic :from string)
  (setf (org-italic-contents italic) (string-trim '(#\/) input)))

;; _underline_
(define-org-object underline () :include plain-text)

(define-org-parser (underline :from string)
  (setf (org-underline-contents underline) (string-trim '(#\_) input)))

;; =verbatim=
(define-org-object verbatim () :include plain-text)

(define-org-parser (verbatim :from string)
  (setf (org-verbatim-contents verbatim) (string-trim '(#\=) input)))

;; ~code~
(define-org-object code () :include plain-text)

(define-org-parser (code :from string)
  (setf (org-code-contents code) (string-trim '(#\~) input)))

;; +strike-through+
(define-org-object strike-through () :include plain-text)

(define-org-parser (strike-through :from string)
  (setf (org-strike-through-contents strike-through) (string-trim '(#\+) input)))
