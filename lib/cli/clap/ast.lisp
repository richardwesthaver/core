;;; cli/clap/ast.lisp --- Clap AST

;; Internal AST representation of CLI objects

;;; Commentary:

;; typically when starting from a top-level CLI, the global
;; CLI-OPTS will be parsed first, followed by the first command
;; found. If a command is found, the tail of the list is passed as
;; arguments to this function, which can pass additonal arguments to
;; nested commands.

;;; Code:
(in-package :cli/clap/ast)

