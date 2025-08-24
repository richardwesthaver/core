;;; ffi/tree-sitter/ffi.lisp --- Low-level FFI bindings for Tree-sitter

;;

;; see https://github.com/death/cl-tree-sitter for an alternative
;; implementation - has functions for working on pointers instead of
;; raw objects like below:

;;(defar ts-node-start-point-pointer ts-point (self (* ts-node)))
;;(defar ts-node-end-point-pointer ts-point (self (* ts-node)))

;;; Code:
(in-package :tree-sitter)

;;; Alien Types
(define-alien-type ts-state-id unsigned-int)
(define-alien-type ts-symbol unsigned-int)
(define-alien-type ts-field-id unsigned-int)
(define-alien-type ts-language (struct ts-language))
(define-alien-type ts-parser (struct ts-parser))
(define-alien-type ts-tree (struct ts-tree))
(define-alien-enum (ts-query-error unsigned-int)
  :none 0
  :syntax 1
  :node-type 2
  :field 3
  :capture 4)

(define-alien-type ts-query 
    (struct ts-query
      ;; ts-query
      (query (* t))
      (error-offset unsigned-int)
      (error-type ts-query-error)))

(define-alien-type ts-query-cursor (struct ts-query-cursor))
(define-alien-type ts-query-cursor-state
    (struct ts-query-cursor-state
      (payload (* t))
      (current-byte-offset unsigned-int)))

(define-alien-type ts-node
    (struct ts-node
            (context (array unsigned-int 4))
            (id (* t))
            (tree (* ts-tree))))

(define-alien-type ts-query-capture
    (struct ts-query-capture
      (node ts-node)
      (index unsigned-int)))

(define-alien-type ts-query-match
    (struct ts-query-match
      (id unsigned-int)
      (pattern-index unsigned-short)
      (capture-count unsigned-short)
      ;; (* ts-query-capture)
      (captures (* t))))

(define-alien-type ts-query-cursor-options 
  (struct ts-query-cursor-options
    (payload (* t))
    (state (* ts-query-cursor-state))))

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
(defar ts-parser-new (* ts-parser))
(defar ts-parser-delete void (self (* ts-parser)))
(defar ts-parser-reset void (self (* ts-parser)))
(defar ts-parser-set-language boolean (self (* ts-parser)) (language (* ts-language)))
(defar ts-parser-language (* ts-language) (self (* ts-parser)))
;; (defar ts-parser-parse (* ts-tree) (self (* ts-parser)) (old-tree (* ts-tree)) (input ts-input))
(defar ts-parser-parse-string (* ts-tree) (self (* ts-parser)) (old-tree (* ts-tree)) (string c-string) (length unsigned-int))
;; Set the file descriptor to which the parser should write debugging graphs
;; during parsing. The graphs are formatted in the DOT language. You may want
;; to pipe these graphs directly to a `dot(1)` process in order to generate
;; SVG output. You can turn off this logging by passing a negative number.
(defar ts-parser-print-dot-graphs void (self (* ts-parser)) (fd int))
;;; Tree
(defar ts-tree-copy (* ts-tree) (self (* ts-tree)))
(defar ts-tree-delete void (self (* ts-tree)))
(defar ts-tree-language (* ts-language) (self (* ts-tree)))
(defar ts-tree-edit void (self (* ts-tree)) (edit (* unsigned-int)))
(defar ts-tree-print-dot-graph void (self (* ts-tree)) (file-descriptor int))

;;; Tree Cursor
(defar ts-tree-cursor-current-field-name c-string (cursor (* ts-tree-cursor)))
(defar ts-tree-cursor-current-field-id ts-field-id (cursor (* ts-tree-cursor)))
(defar ts-tree-cursor-current-descendant-index unsigned-int (cursor (* ts-tree-cursor)))
(defar ts-tree-cursor-current-depth unsigned-int (cursor (* ts-tree-cursor)))
(defar ts-tree-cursor-goto-next-sibling boolean (self (* ts-tree-cursor)))
(defar ts-tree-cursor-goto-first-child-for-byte long (self (* ts-tree-cursor)) (goal-byte unsigned-int))
(defar ts-tree-cursor-goto-first-child-for-point-pointer long (self (* ts-tree-cursor)) (goal-point (* ts-point)))
(defar ts-tree-cursor-goto-parent boolean (self (* ts-tree-cursor)))
(defar ts-tree-cursor-copy-pointer (* ts-tree-cursor) (cursor (* ts-tree-cursor)))
(defar ts-tree-cursor-goto-first-child boolean (self (* ts-tree-cursor)))
(defar ts-tree-cursor-goto-last-child boolean (self (* ts-tree-cursor)))
(defar ts-tree-cursor-goto-descendant void (self (* ts-tree-cursor)) (goal-descendant-index unsigned-int))
(defar ts-tree-cursor-delete void (cursor (* ts-tree-cursor)))

(defar ts-language-version unsigned-int (v (* ts-language)))
(defar ts-language-symbol-count unsigned-int (v (* ts-language)))
(defar ts-language-symbol-name c-string (v (* ts-language)) (s (* ts-symbol)))
(defar ts-language-symbol-type ts-symbol-type (v (* ts-language)) (s ts-symbol))
(defar ts-language-field-count unsigned-int (v (* ts-language)))
(defar ts-language-field-name-for-id c-string (v (* ts-language)) (id ts-field-id))
(defar ts-language-field-id-for-name ts-field-id (v (* ts-language)) (name c-string) (nlen (unsigned 32)))

(defar ts-language-next-state ts-state-id 
  (self (* ts-language)) 
  (state ts-state-id)
  (symbol ts-symbol))

;;; Query
(defar ts-query-new (* ts-query)
  (lang (* ts-language))
  (source c-string)
  (source-len unsigned-int)
  (error-offset (* unsigned-int))
  (error-type (* ts-query-error)))

(defar ts-query-delete void (query (* ts-query)))

(defar ts-query-cursor-new (* ts-query-cursor))

(defar ts-query-cursor-delete void
  (cursor (* ts-query-cursor)))

(defar ts-query-pattern-count unsigned-int
  (self (* ts-query)))
(defar ts-query-capture-count unsigned-int
  (self (* ts-query)))
(defar ts-query-string-count unsigned-int
  (self (* ts-query)))

;;; ALIEN.C
(defar ts-query-cursor-exec-pointer void
  (cursor (* ts-query-cursor))
  (query (* ts-query))
  (node (* ts-node)))

(defar ts-query-cursor-exec-with-options-pointer void
  (cursor (* ts-query-cursor))
  (query (* ts-query))
  (node (* ts-node))
  (options (* ts-query-cursor-options)))

(defar ts-query-cursor-next-match boolean
  (cursor (* ts-query-cursor))
  (match (* ts-query-match)))

(defar ts-query-cursor-remove-match void
  (cursor (* ts-query-cursor))
  (match-id unsigned-int))

(defar ts-query-cursor-next-capture boolean
  (cursor (* ts-query-cursor))
  (match (* ts-query-match))
  (capture-index (* unsigned-int)))

(defar ts-query-cursor-set-max-start-depth void
  (cursor (* ts-query-cursor))
  (max-start-depth unsigned-int))

(defar ts-tree-root-node-pointer (* ts-node)
  (tree (* ts-tree)))

(defar ts-tree-cursor-new-pointer (* ts-tree-cursor)
  (node (* ts-node)))

(defar ts-node-is-named-pointer boolean
  (node (* ts-node)))

(defar ts-tree-cursor-current-node-pointer (* ts-node)
  (cursor (* ts-tree-cursor)))

(defar ts-node-start-point-pointer (* ts-point)
  (node (* ts-node)))

(defar ts-node-end-point-pointer (* ts-point)
  (node (* ts-node)))

(defar ts-node-type-pointer c-string
  (node (* ts-node)))

(defar ts-node-string-pointer c-string
  (node (* ts-node)))

(defar ts-node-start-byte-pointer unsigned-int
  (node (* ts-node)))

(defar ts-node-end-byte-pointer unsigned-int
  (node (* ts-node)))

(defar ts-node-child-count-pointer unsigned-int
  (node (* ts-node)))

(defar ts-node-parent-pointer (* ts-node)
  (node (* ts-node)))

