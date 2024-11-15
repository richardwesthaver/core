;;; ffi/tree-sitter/pkg.lisp --- Tree-sitter FFI

;; Tree-sitter FFI for Lisp

;; Tree-sitter consists of the base library, which you can load using
;; the LOAD-TREE-SITTER function, and the language parsers.

;; The language parser shared libraries should ALWAYS be located in
;; /usr/local/lib/ and be prefixed with 'lib' like so:
;; '/usr/local/lib/libtree-sitter-json.so'. Static libraries are not
;; supported.

;; The language parsers have associated json files which should ALWAYS
;; be stored in subdirectories of /usr/local/share/tree-sitter/ like
;; so: '/usr/local/share/tree-sitter/json/grammar.json'.

;;; Code:
(defpackage :tree-sitter
  (:use :cl :std :sb-alien :std/alien)
  (:export 
   :load-tree-sitter
   :load-tree-sitter-alien
   :tree-sitter-language-files
   :*ts-langs*
   :list-ts-langs
   :*tree-sitter-language-directory*
   :ts-state-id
   :ts-symbol
   :ts-field-id
   :ts-language
   :ts-parser
   :ts-tree
   :ts-query
   :ts-query-cursor
   :ts-query-error
   :ts-lookahead-iterator
   :ts-point
   :ts-logger
   :ts-node
   :ts-tree-cursor
   :ts-parser-new
   :ts-parser-delete
   :ts-parser-reset
   :ts-parser-logger
   :ts-parser-set-logger
   :ts-parser-set-language
   :ts-parser-language
   :ts-parser-parse
   :ts-parser-parse-string
   :ts-parser-print-dot-graphs
   :ts-tree-copy
   :ts-tree-delete
   :ts-tree-language
   :ts-tree-edit
   :ts-tree-print-dot-graph
   :ts-node-type
   :ts-node-symbol
   :ts-node-language
   :ts-node-grammar-type
   :ts-node-grammar-symbol
   :ts-node-start-byte
   :ts-node-start-point
   :ts-node-end-byte
   :ts-node-end-point
   :ts-node-string
   :ts-node-is-null
   :ts-node-eq
   :ts-node-named-child
   :ts-tree-cursor-new-pointer
   :ts-tree-cursor-delete
   :ts-tree-root-node
   :ts-language-version
   :ts-language-symbol-name
   :ts-language-symbol-count
   :ts-language-field-count
   :ts-tree-cursor-current-field-name
   :ts-tree-cursor-goto-next-sibling
   :ts-tree-cursor-goto-parent
   :ts-tree-cursor-goto-first-child
   :ts-query-new
   :ts-query-delete
   :ts-tree-root-node-pointer
   :ts-node-is-named-pointer
   :ts-tree-cursor-current-node-pointer
   :ts-node-start-point-pointer
   :ts-node-end-point-pointer
   :ts-node-type-pointer
   :with-ts-parser
   :with-ts-lang
   :language-module
   :parse-string-with-language
   :tree-sitter-error
   :with-ts-node
   :parse-string
   :with-ts-cursor))

(in-package :tree-sitter)

(define-alien-loader tree-sitter)
(define-alien-loader tree-sitter-alien)
