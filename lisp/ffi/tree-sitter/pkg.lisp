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
  (:nicknames :ts)
  (:use :cl :std :sb-alien)
  (:export 
   :load-tree-sitter
   :load-tree-sitter-wrapper
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
   :ts-tree-root-node
   :ts-language-version
   :ts-language-symbol-name
   :ts-language-symbol-count
   :ts-language-field-count))

(in-package :tree-sitter)

(defun load-tree-sitter () 
  (unless (member :tree-sitter *features*)
    (sb-alien:load-shared-object (shared-object-name "tree-sitter") :dont-save t)
    (load-tree-sitter-wrapper)
    (push :tree-sitter *features*)))

(defun load-tree-sitter-wrapper ()
  (handler-bind ((simple-error
                   (lambda (condition)
                     (warn "failed to load libtree-sitter-wrapper.so --- make sure to follow the install instructions in ffi/tree-sitter/wrapper.c! ~a" condition))))
    (sb-alien:load-shared-object "/usr/local/lib/libtree-sitter-wrapper.so" :dont-save t)))
