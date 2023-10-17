(pkg:defpkg :tree-sitter
  (:use :cl :alien))

(in-package :tree-sitter)

(defun load-tree-sitter () 
  (unless (member :tree-sitter *features*)
    (sb-alien:load-shared-object "libtree-sitter.so" :dont-save t)
    (push :tree-sitter *features*)))

(load-tree-sitter)

;;; Alien Types
(define-alien-type ts-state-id unsigned-int)
(define-alien-type ts-symbol unsigned-int)
(define-alien-type ts-field-id unsigned-int)
(define-alien-type ts-language (struct ts-language))
(define-alien-type ts-parser (struct ts-parser))
(define-alien-type ts-tree (struct ts-tree))
;; not thread-safe
(define-alien-type ts-query (struct ts-query))
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
    (struct nil
	    (context (array unsigned-int 4))
	    (id (* t))
	    (tree (* ts-tree))))
(define-alien-type ts-tree-cursor 
    (struct nil
	    (tree (* t))
	    (id (* t))
	    (context (array unsigned-int 2))))
;;; Parser
(define-alien-routine ts-parser-new (* ts-parser))
(define-alien-routine ts-parser-delete void (self (* ts-parser)))
(define-alien-routine ts-parser-reset void (self (* ts-parser)))
(define-alien-routine ts-parser-logger ts-logger (self (* ts-parser)))
(define-alien-routine ts-parser-set-logger void (self (* ts-parser)) (logger ts-logger))
(define-alien-routine ts-parser-language (* ts-language) (self (* ts-parser)))
(define-alien-routine ts-parser-parse (* ts-tree) (self (* ts-parser)) (old-tree (* ts-tree)) (length unsigned-int))
(define-alien-routine ts-parser-parse-string (* ts-tree) (self (* ts-parser)) (string c-string) (length unsigned-int))
;; Set the file descriptor to which the parser should write debugging graphs
;; during parsing. The graphs are formatted in the DOT language. You may want
;; to pipe these graphs directly to a `dot(1)` process in order to generate
;; SVG output. You can turn off this logging by passing a negative number.
(define-alien-routine ts-parser-print-dot-graphs void (self (* ts-parser)) (fd int))
;;; Tree
(define-alien-routine ts-tree-copy (* ts-tree) (self (* ts-tree)))
(define-alien-routine ts-tree-delete void (self (* ts-tree)))
(define-alien-routine ts-tree-language (* ts-language) (self (* ts-tree)))
(define-alien-routine ts-tree-edit void (self (* ts-tree)) (edit (* unsigned-int)))
(define-alien-routine ts-tree-print-dot-graph void (self (* ts-tree)) (file-descriptor int))
;;; Node
(define-alien-routine ts-node-type c-string (self ts-node))
(define-alien-routine ts-node-symbol ts-symbol (self ts-node))
(define-alien-routine ts-node-language (* ts-language) (self ts-node))
(define-alien-routine ts-node-grammar-type c-string (self ts-node))
(define-alien-routine ts-node-grammar-symbol ts-symbol (self ts-node))
(define-alien-routine ts-node-start-byte unsigned-int (self ts-node))
;; (define-alien-routine ts-node-start-point ts-point (self ts-node))
(define-alien-routine ts-node-end-byte unsigned-int (self ts-node))
;; (define-alien-routine ts-node-end-point ts-point (self ts-node))
(define-alien-routine ts-node-string c-string (self ts-node))
(define-alien-routine ts-node-is-null boolean (self ts-node))
(define-alien-routine ts-node-eq boolean (self ts-node) (other ts-node))
;;; Tree Cursor
(define-alien-routine ts-tree-cursor-new ts-tree-cursor (node ts-node))