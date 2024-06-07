;;; cli/clap/ast.lisp --- Clap AST

;; 

;;; Code:
(in-package :cli/clap/ast)

;; typically when starting from a top-level CLI, the global
;; CLI-OPTS will be parsed first, followed by the first command
;; found. If a command is found, the tail of the list is passed as
;; arguments to this function, which can pass additonal arguments to
;; nested commands.

;;  TODO 2023-09-12: Parsing restarts at the `*cli-group-separator*'
;; if present, or stops at EOI.
(defstruct (cli-node (:constructor make-cli-node (kind form))) kind form)

(defstruct (cli-ast (:constructor make-cli-ast (ast))) ast)
