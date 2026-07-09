;;; alien/tree-sitter/pkg.lisp --- Tree-sitter FFI

;; Tree-sitter FFI for Lisp

;; Tree-sitter consists of the base library, which you can load using
;; the LOAD-TREE-SITTER function, and the language parsers.

;; The language parser shared libraries should ALWAYS be located in
;; /usr/lib/ and be prefixed with 'lib' like so:
;; '/usr/lib/libtree-sitter-json.so'. Static libraries are not
;; supported.

;; The language parsers have associated json files which should ALWAYS
;; be stored in subdirectories of /usr/share/tree-sitter/ like
;; so: '/usr/share/tree-sitter/json/grammar.json'.

;;; Code:
(defpackage :tree-sitter
  (:use :cl :std :sb-alien :std/alien)
  (:export 
   :load-tree-sitter
   :tree-sitter-c
   :load-tree-sitter-c
   :tree-sitter-rust
   :load-tree-sitter-rust
   :tree-sitter-bash
   :load-tree-sitter-bash
   :tree-sitter-cpp
   :load-tree-sitter-cpp
   :tree-sitter-go
   :load-tree-sitter-go
   :tree-sitter-javascript
   :load-tree-sitter-javascript
   :tree-sitter-python
   :load-tree-sitter-python
   :tree-sitter-typescript-tsx
   :load-tree-sitter-typescript-tsx
   :tree-sitter-typescript-typescript
   :load-tree-sitter-typescript-typescript
   :tree-sitter-yaml
   :load-tree-sitter-yaml
   :+tree-sitter-language-version+
   :+tree-sitter-min-compatible-language-version+
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
   :ts-tree-cursor-new
   :ts-tree-cursor-delete
   :ts-tree-root-node
   :ts-language-abi-version
   :ts-language-name
   :ts-language-symbol-name
   :ts-language-symbol-count
   :ts-language-field-count
   :ts-tree-cursor-current-field-name
   :ts-tree-cursor-goto-next-sibling
   :ts-tree-cursor-goto-parent
   :ts-tree-cursor-goto-first-child
   :ts-query-new
   :ts-query-delete
   :ts-tree-root-node
   :ts-node-is-named
   :ts-tree-cursor-current-node
   :ts-node-start-point
   :ts-node-end-point
   :ts-node-type
   :with-ts-parser
   :with-ts-lang
   :language-module
   :parse-string-with-language
   :tree-sitter-error
   :with-ts-node
   :parse-lang-string
   :with-ts-cursor
   :convert-ts-tree
   :ts-language-next-state
   :ts-language-field-id-for-name
   :ts-language-field-name-for-id
   :ts-language-symbol-type
   :with-ts-query
   :ts-query-cursor-delete
   :with-ts-query-cursor
   :check-ts-query-error))

(in-package :tree-sitter)

(define-alien-loader tree-sitter)
