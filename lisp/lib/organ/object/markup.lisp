;;; lib/organ/object/markup.lisp --- Org Markup

;; includes PLAIN-TEXT, BOLD, ITALIC, UNDERLINE, VERBATIM, CODE, and STRIKE-THROUGH.

;;; Code:
(in-package :organ)

;; Any string that doesn't match another object is considered a plain
;; text object. Whitespace MAY be collapsed within any plain-text
;; instance.
(defmacro extract-with-markup (char input)
  `(with-lexer-environment (,input)
     (when (char= ,char (consume))
       (consume-until (make-matcher (is ,char))))))

(define-org-object plain-text (contents))

(define-org-parser (plain-text :from string)
  (let ((res (org-create :plain-text)))
    (setf (org-plain-text-contents res) input)
    res))

;; *bold*
(define-org-object bold () :include plain-text)

(define-org-parser (bold :from string)
  (let ((res (org-create :bold)))
    (setf (org-bold-contents res) 
          (extract-with-markup #\* input))
    res))

;; /italic/
(define-org-object italic () :include plain-text)

(define-org-parser (italic :from string)
  (let ((res (org-create :italic)))
    (setf (org-italic-contents res) 
          (extract-with-markup #\/ input))
    res))

;; _underline_
(define-org-object underline () :include plain-text)

(define-org-parser (underline :from string)
  (let ((res (org-create :underline)))
    (setf (org-underline-contents res) 
          (extract-with-markup #\_ input))
    res))

;; =verbatim=
(define-org-object verbatim () :include plain-text)

;; FIXME 2023-12-27: 
(define-org-parser (verbatim :from string)
  (let ((res (org-create :verbatim)))
    (setf (org-verbatim-contents res)
          (extract-with-markup #\= input))
    res))

;; ~code~
(define-org-object code () :include plain-text)

(define-org-parser (code :from string)
  (let ((res (org-create :code)))
    (setf (org-code-contents res)
          (extract-with-markup #\~ input))
    res))

;; +strike-through+
(define-org-object strike-through () :include plain-text)

(define-org-parser (strike-through :from string)
  (let ((res (org-create :strike-through)))
    (setf (org-strike-through-contents res)
          (extract-with-markup #\+ input))
    res))
