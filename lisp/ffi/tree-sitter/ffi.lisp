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
(define-alien-type ts-language (struct ts-language))
(define-alien-type ts-parser (struct ts-parser))
(define-alien-type ts-tree (struct ts-tree))
;; not thread-safe
(define-alien-type ts-query (struct ts-query))
(define-alien-type ts-query-error unsigned-int)
(define-alien-type ts-query-cursor (struct ts-query-cursor))
(define-alien-type ts-lookahead-iterator (struct ts-lookahead-iterator))
(define-alien-type ts-point
    (struct ts-point
	    (row unsigned-int)
	    (column unsigned-int)))
(define-alien-type ts-range
  (struct ts-range
          (start-point ts-point)
          (end-point ts-point)
          (start-byte unsigned-int)
          (end-byte unsigned-int)))
          
(define-alien-enum (ts-log-type int)
                   :parse 0
                   :lex 1)

(define-alien-type ts-logger
    (struct nil
	    (payload (* t))
	    (log (* (function void (* t) ts-log-type c-string)))))

(define-alien-type ts-input-edit
  (struct ts-input-edit
          (start-byte unsigned-int)
          (old-end-byte unsigned-int)
          (new-end-byte unsigned-int)
          (start-point ts-point)
          (old-end-point ts-point)
          (new-end-point ts-point)))

(define-alien-type ts-node
    (struct ts-node
	    (context (array unsigned-int 4))
	    (id (* t))
	    (tree (* ts-tree))))

(define-alien-type ts-tree-cursor 
    (struct nil
	    (tree (* ts-tree))
	    (id (* t))
	    (context (array unsigned-int 2))))

(define-alien-enum (ts-input-encoding int)
                   :utf-8 0
                   :utf-16 2)

(define-alien-enum (ts-symbol-type int)
                   :regular 0
                   :anonymous 1
                   :auxiliary 2)

(define-alien-type ts-input (struct ts-input
                                    (payload (* t))
                                    (read (* (function c-string (* t)
                                                       unsigned-int
                                                       ts-point
                                                       (* unsigned-int))))
                                    (encoding ts-input-encoding)))

;;; Parser
(define-alien-routine ts-parser-new (* ts-parser))
(define-alien-routine ts-parser-delete void (self (* ts-parser)))
(define-alien-routine ts-parser-reset void (self (* ts-parser)))
(define-alien-routine ts-parser-set-language boolean (self (* ts-parser)) (language (* ts-language)))
(define-alien-routine ts-parser-language (* ts-language) (self (* ts-parser)))
;; (define-alien-routine ts-parser-parse (* ts-tree) (self (* ts-parser)) (old-tree (* ts-tree)) (input ts-input))
(define-alien-routine ts-parser-parse-string (* ts-tree) (self (* ts-parser)) (old-tree (* ts-tree)) (string c-string) (length unsigned-int))
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

;;; Tree Cursor
(define-alien-routine ts-tree-cursor-current-field-name c-string (cursor (* ts-tree-cursor)))

(define-alien-routine ts-tree-cursor-goto-next-sibling boolean (self (* ts-tree-cursor)))

(define-alien-routine ts-tree-cursor-goto-parent boolean (self (* ts-tree-cursor)))

(define-alien-routine ts-tree-cursor-goto-first-child boolean (self (* ts-tree-cursor)))

(define-alien-routine ts-tree-cursor-delete void (cursor (* ts-tree-cursor)))

(define-alien-routine ts-language-version unsigned-int (v (* ts-language)))
(define-alien-routine ts-language-symbol-count unsigned-int (v (* ts-language)))
(define-alien-routine ts-language-symbol-name c-string (v (* ts-language)) (s (* ts-symbol)))
(define-alien-routine ts-language-field-count unsigned-int (v (* ts-language)))

;;; Query
(define-alien-routine ts-query-new (* ts-query)
  (lang (* ts-language))
  (source (* char))
  (source-len unsigned-int)
  (error-offset (* unsigned-int))
  (error-type (* ts-query-error)))

(define-alien-routine ts-query-delete void (query (* ts-query)))

;;; ALIEN.C
(define-alien-routine ts-tree-root-node-pointer (* ts-node)
  (tree (* ts-tree)))

(define-alien-routine ts-tree-cursor-new-pointer (* ts-tree-cursor)
  (node (* ts-node)))

(define-alien-routine ts-node-is-named-pointer boolean
  (node (* ts-node)))

(define-alien-routine ts-tree-cursor-current-node-pointer (* ts-node)
  (cursor (* ts-tree-cursor)))

(define-alien-routine ts-node-start-point-pointer (* ts-point)
  (node (* ts-node)))

(define-alien-routine ts-node-end-point-pointer (* ts-point)
  (node (* ts-node)))

(define-alien-routine ts-node-type-pointer c-string
  (node (* ts-node)))
