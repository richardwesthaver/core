;;; apl.lisp --- Experimental support for APL syntax family

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

;; ref: https://aplwiki.com/wiki/APL_syntax

;;; Code:
(defpackage :syn/lang/apl
  (:nicknames :syn/apl)
  (:use :cl :std :syn/lang :parse/pratt :ast)
  (:export))
(in-package :syn/lang/apl)

(defclass apl-program (ast) ((env)))

(defclass k-program (apl-program) ())
