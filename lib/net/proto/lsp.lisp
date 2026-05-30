;;; lsp.lisp --- Language Server Protocol

;; LSP support for CL

;;; Commentary:

;; LSP is the standard means of providing language context for IDEs on a
;; per-project basis. Of course CL has traditionally never adapted to this
;; trend and always implemented IDE features on Swank/SLIME, I think it would
;; be nice if they could work together.

;; To this end our goal is to provide a CL language server spawned via SKEL
;; which also hosts a Swank server, without duplicating functionality.

;;; Code:
(in-package :net/proto/lsp)
