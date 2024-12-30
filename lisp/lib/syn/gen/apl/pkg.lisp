;;; syn/apl/pkg.lisp --- Experimental support for APL syntax families

;; This module is experimental while I get more familiar with the full set of
;; APL primitives vs K and decide on the API. which will determine ultimately
;; where it lives in the source tree.

;;; Commentary:

;; APL represents the final component envisioned in our Language Tower. It is
;; a family of language that is better suited to tricky vector-based
;; operations than any other.

;; APL Programs are short, because APL Primitives are single characters. In
;; the case of K the full range of primiitives is covereed by the ascii
;; character set and no special keyboard suport is required. In Standard APL,
;; J, BQN, etc many special UTF-8 characters are used.

;; K has been a source of inspiration for me for many years, along with the
;; musings of ATW. This was my first introduction to this incredibly terse way
;; of writing code and interpreting it. It is simply thrilling when the nature
;; of a few characters reveals themselves to be the equivalent of a few
;; hundred lines of C or tens of lines of Python.

;;;; Learning from April

;; April is a much more complete implementation of APL that compiles to Common
;; Lisp. It's very cool and I still have much more to learn from their work.

;; There isn't much code I can actually reuse from April since I'm favoring an
;; interpreted (or partially interpreted) design and depending on my own set
;; of CLOS objects and protocols. 

;; I do find the VARRAYs concept quite interesting though - need more time to
;; study it and see if it is something that we can leverage as part of a
;; Vector-based VM and expose in IO.

;; Once we have a stable AST we will need to expose an API for specifying how
;; to execute APL statements and programs. Ideally, there isn't much
;; boilerplate needed and we can focus on defining the underlying data
;; structures while getting a highly advanced APL runtime to hack away on it
;; with.

;; ref: https://github.com/phantomics/april

;; ref: https://xpqz.github.io/learnapl/aplway.html
;; ref: https://aplwiki.com/wiki/Tacit_programming
;; ref: https://xpqz.github.io/learnapl/tacit.html
;; ref: https://aplwiki.com/wiki/APL_syntax
;; ref: https://dfns.dyalog.com/n_contents.htm
;; ref: https://docs.dyalog.com/latest/Dyalog%20Programming%20Reference%20Guide.pdf

;;; Code:
(defpackage :syn/gen/apl
  (:nicknames :syn/apl)
  (:use :cl :std :syn/lang :parse/pratt :ast :syn/gen :id)
  (:export
   :apl-reader
   :apl-syntax
   :read-apl-string
   :read-apl-file))

(defpackage syn/gen/apl/swap)

(in-package :syn/apl)

(defvar *apl-backend*
  (append *cl-symbols*
          '(apl-op apl-array-type apl-program apl-evaluator)))

(export *apl-backend*)

(defparameter *apl-glyphs* nil)

(defparameter *apl-syntax* nil)

(defun apl-glyph (sym names)
  (pushnew sym *apl-glyphs*)
  (mapc (lambda (x) (pushnew x *apl-syntax*)) names))

(defmacro apl-glyphs (&body forms)
  `(progn
     ,@(mapcar (lambda (x) 
                 (destructuring-bind (sym &rest names) x
                   `(apl-glyph ',sym ',names)))
               forms)))
(apl-glyphs 
  (← assign)
  (→ branch)
  ($ if)
  (⍢ variant) ;; (⍠ variant) ;; dyalog
  (\: guard)
  (⍝ comment)
  (/ replicate reduce) 
  (⌿ replicate-first reduce-first)
  (\\ expand scan) 
  (⍀ expand-first scan-first) 
  (¨ each) 
  (⍤ atop rank)
  (⍛ before)
  (⍥ over) 
  (⌸ key) 
  (⌺ stencil)
  (⍨ commute) 
  (⍣ power) 
  (\. product)
  (∘ outer-product beside) 
  (@ at) 
  (& spawn) 
  (⌶ i-beam) 
  (+ add conj)
  (- negate sub)
  (× sign mul)
  (÷ recip div)
  (⌊ floor min)
  (⌈ ceil max)
  (\| mag res)
  (* exponential power)
  (⍟ natural-log log)
  (○ pi-times circular)
  (! fact binom) 
  (≠ unique-mask neq )
  (~ not without) 
  (? random deal) 
  (∊ enlist membership)
  (\, ravel catenate laminate)
  (⍪ table catenate-first) 
  (⌷ index) 
  (⍳ interval index-of) 
  (⍸ where interval-index)
  (⍴ shape reshape)
  (↑ mix take) 
  (↓ split drop) 
  (⊣ empty left) 
  (⊢ identity right) 
  (⌽ reverse rotate) 
  (⊖ reverse-first rotate-first) 
  (⍉ transpose permute) 
  (⍋ grade-up grade-up-by) 
  (⍒ grace-down grace-down-by) 
  (⌹ matrix-inverse matrix-divide) 
  (≡ depth match) 
  (≢ first-dim not-match) 
  (⊂ enclose partitioned-enclose)
  (⊆ nest partition) 
  (⊃ disclose pick) 
  (∪ unique union) 
  (⍎ evaluate) 
  (⍕ format format-at-precision) 
  (∧ and) 
  (∨ or)
  (⍲ nand)
  (⍱ nor)
  (< lt) 
  (≤ llteq)
  (= eq) 
  (≥ gteq)
  (> gt) 
  (⍷ find) 
  (⊤ encode) 
  (⊥ decode) 
  (∩ intersection))
  
(defparameter *apl-exports*
  (append *apl-glyphs*
          *apl-syntax*))

(defparameter *apl-swap*
  (append *apl-glyphs* *apl-syntax*))

(pkg:defpackage* :syn/gen/apl/sym
    (:shadow-symbols *apl-symbols* :export-symbols *apl-exports*)
  (:nicknames :apl)
  (:use :cl)
  (:import-from :syn/apl :apl-reader :read-apl-string :read-apl-file))

(defclass apl-program (ast) ((env)))

;; arrays
;; functions (monadic or dyadic)
;; monadic-op -> derived-function
;; dyadic-op -> derived-function
;; niladic-op -> immediate eval to one of above values
;; hyperators?
