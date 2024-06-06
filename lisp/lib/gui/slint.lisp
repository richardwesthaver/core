;;; gui/slint.lisp --- Slint UI Compiler

;; This package provides a simple compiler for .slint files.

;; see also: core/rust/lib/ui and core/rust/ui

;;; Commentary:

;; The DSL is S-expression based and can be extended with macros.

;; https://releases.slint.dev/1.5.1/docs/slint/ - Language Docs

;; https://releases.slint.dev/1.5.1/docs/rust/slint/ - Rust API Docs

;; https://slintpad.com/ - online playground

;;; Code:
(in-package :gui/slint)

(define-matcher slint-import-statement
    (matcher-string "import"))

(define-grammar *slint-grammar*
  (:start-symbol expr)
  (expr
   tok)
  (tok t))

;; (with-lexer-environment ...)
;; (parse-with-lexer #'terpri (make-parser *slint-grammar*))

(defmacro with-slint (&body body) `(progn ,@body nil))
(defun compile-slint (slint-expr &key output)
  "Compile the SLINT-EXPR to a Slint UI definition, optionally sending to
OUTPUT."
  nil)
