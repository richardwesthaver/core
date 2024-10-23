;;; cl.lisp --- Common Lisp Code Generator

;; A 'translating' codegen layer from the CC Core libraries to our own flavor
;; of Reasonably Portable Common Lisp.

;;; Commentary:

;; The CC software stack is a private island and it can be difficult to
;; reasonably share snippets of code externally. This package is designed to
;; transpile source code which depends on our internal libraries to Portable
;; Common Lisp.

;; Unlike the other Code Generators this one is really just scaffolding around
;; MACROEXPAND.

;;; Code:
(in-package :syn/gen/lisp/cl)
