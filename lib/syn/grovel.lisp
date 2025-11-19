;;; grovel.lisp --- Tree-sitter-based groveler

;; Targeting C for now, but possibly extensible to other tree-sitter-parseable
;; langs.

;;; Commentary:

;; Similar to how STD/DEFSYS is reasonably compatible with ASDF, SYN/GROVEL is
;; reasonable compatible with SB-GROVEL.

;; We intend to use SB-GROVEL for FFI up until the point that we are able to
;; load this file - then we can replace those modules with SYN/GROVEL
;; equivalents while fully productionizing a Lisp image before saving it.

#| sh
gcc alien.c -E -dD -P > defs.i
|#

;; The main difference between SB-GROVEL and SYN/GROVEL is that we only use
;; the C compiler to pre-process input files. We do NOT need to generate our
;; own C code, compile it, and execute it as SB-GROVEL does - instead we take
;; the pre-processed input and run it through tree-sitter (via SYN/TS) to
;; generate an internal AST which we can query from Lisp. This allows us to
;; define much more precise selectors and transformers to be used in
;; generating Lisp code from the AST.

;;; Code:
(in-package :syn/grovel)
