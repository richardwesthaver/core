;;; ffi/tree-sitter/ffi.lisp --- Low-level FFI bindings for Tree-sitter

;;

;; see https://github.com/death/cl-tree-sitter for an alternative
;; implementation - has functions for working on pointers instead of
;; raw objects like below:

;;(define-alien-routine ts-node-start-point-pointer ts-point (self (* ts-node)))
;;(define-alien-routine ts-node-end-point-pointer ts-point (self (* ts-node)))

;;; Code:
(in-package :tree-sitter)

;;; Alien Types
(define-alien-type ts-state-id unsigned-int)
(define-alien-type ts-symbol unsigned-int)
(define-alien-type ts-field-id unsigned-int)
(define-alien-type ts-language (* t))
(define-alien-type ts-parser (* t))
(define-alien-type ts-tree (* t))
;; not thread-safe
(define-alien-type ts-query (struct ts-query))
(define-alien-type ts-query-error unsigned-int)
(define-alien-type ts-query-cursor (struct ts-query-cursor))
(define-alien-type ts-lookahead-iterator (struct ts-lookahead-iterator))
(define-alien-type ts-point
    (struct nil
	    (row unsigned-int)
	    (column unsigned-int)))
(define-alien-type ts-logger
    (struct nil
	    (payload (* t))
	    (log (* t))))
(define-alien-type ts-node
    (struct ts-node
	    (context (array unsigned-int 4))
	    (id (* t))
	    (tree (* t))))
(define-alien-type ts-tree-cursor 
    (struct nil
	    (tree (* t))
	    (id (* t))
	    (context (array unsigned-int 2))))

;;; Parser
(define-alien-routine ts-parser-new ts-parser)
(define-alien-routine ts-parser-delete void (self ts-parser))
(define-alien-routine ts-parser-reset void (self ts-parser))
(define-alien-routine ts-parser-logger ts-logger (self ts-parser))
(define-alien-routine ts-parser-set-logger void (self ts-parser) (logger ts-logger))

(define-alien-routine ts-parser-set-language boolean (self ts-parser) (language ts-language))

(define-alien-routine ts-parser-language ts-language (self ts-parser))
(define-alien-routine ts-parser-parse ts-tree (self ts-parser) (old-tree ts-tree) (length unsigned-int))
(define-alien-routine ts-parser-parse-string ts-tree (self ts-parser) (old-tree ts-tree) (string c-string) (length unsigned-int))
;; Set the file descriptor to which the parser should write debugging graphs
;; during parsing. The graphs are formatted in the DOT language. You may want
;; to pipe these graphs directly to a `dot(1)` process in order to generate
;; SVG output. You can turn off this logging by passing a negative number.
(define-alien-routine ts-parser-print-dot-graphs void (self ts-parser) (fd int))
;;; Tree
(define-alien-routine ts-tree-copy ts-tree (self ts-tree))
(define-alien-routine ts-tree-delete void (self ts-tree))
(define-alien-routine ts-tree-language ts-language (self ts-tree))
(define-alien-routine ts-tree-edit void (self ts-tree) (edit (* unsigned-int)))
(define-alien-routine ts-tree-print-dot-graph void (self ts-tree) (file-descriptor int))
(define-alien-routine ts-tree-root-node ts-node (self ts-tree))

;;; Node
(define-alien-routine ts-node-type c-string (self ts-node))
(define-alien-routine ts-node-symbol ts-symbol (self ts-node))
(define-alien-routine ts-node-language ts-language (self ts-node))
(define-alien-routine ts-node-grammar-type c-string (self ts-node))
(define-alien-routine ts-node-grammar-symbol ts-symbol (self ts-node))
(define-alien-routine ts-node-start-byte unsigned-int (self ts-node))
(define-alien-routine ts-node-start-point ts-point (self ts-node))
(define-alien-routine ts-node-end-byte unsigned-int (self ts-node))
(define-alien-routine ts-node-end-point ts-point (self ts-node))
(define-alien-routine ts-node-string c-string (self ts-node))
(define-alien-routine ts-node-is-null boolean (self ts-node))
(define-alien-routine ts-node-is-named boolean (self ts-node))
(define-alien-routine ts-node-is-missing boolean (self ts-node))
(define-alien-routine ts-node-is-extra boolean (self ts-node))
(define-alien-routine ts-node-has-changes boolean (self ts-node))
(define-alien-routine ts-node-has-error boolean (self ts-node))
(define-alien-routine ts-node-parent ts-node (self ts-node))
(define-alien-routine ts-node-child ts-node (self ts-node) (cid unsigned-int))
(define-alien-routine ts-node-named-child ts-node (self ts-node) (cid unsigned-int))
(define-alien-routine ts-node-eq boolean (self ts-node) (other ts-node))
;;; Tree Cursor
(define-alien-routine ts-tree-cursor-new ts-tree-cursor (node ts-node))

(define-alien-routine ts-tree-cursor-current-node ts-node (cursor (* ts-tree-cursor)))

(define-alien-routine ts-tree-cursor-current-field-name c-string (cursor (* ts-tree-cursor)))

(define-alien-routine ts-tree-cursor-goto-next-sibling boolean (self (* ts-tree-cursor)))

(define-alien-routine ts-tree-cursor-goto-parent boolean (self (* ts-tree-cursor)))

(define-alien-routine ts-tree-cursor-goto-first-child boolean (self (* ts-tree-cursor)))

(define-alien-routine ts-tree-cursor-delete void (cursor (* ts-tree-cursor)))

(define-alien-routine ts-language-version unsigned-int (v ts-language))
(define-alien-routine ts-language-symbol-count unsigned-int (v ts-language))
(define-alien-routine ts-language-symbol-name c-string (v ts-language) (s ts-symbol))
(define-alien-routine ts-language-field-count unsigned-int (v ts-language))

;;; Query
(define-alien-routine ts-query-new (* ts-query)
  (lang ts-language)
  (source (* char))
  (source-len unsigned-int)
  (error-offset (* unsigned-int))
  (error-type (* ts-query-error)))

(define-alien-routine ts-query-delete void (query (* ts-query)))

;;; WRAPPER.C
(define-alien-routine ts-tree-root-node-pointer (* ts-node)
  (tree ts-tree))

(define-alien-routine ts-tree-cursor-new-pointer (* ts-node)
  (node (* (struct ts-node))))

(define-alien-routine ts-node-is-named-pointer boolean
  (node (* (struct ts-node))))

(define-alien-routine ts-tree-cursor-current-node-pointer (* ts-node)
  (cursor (* (struct ts-tree-cursor))))

(define-alien-routine ts-node-start-point-pointer (* ts-node)
  (node (* (struct ts-node))))

(define-alien-routine ts-node-end-point-pointer (* ts-node)
  (node (* (struct ts-node))))

(define-alien-routine ts-node-type-pointer c-string
  (node (* (struct ts-node))))
