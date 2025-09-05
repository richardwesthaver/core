;;; cli/clap/ast.lisp --- Clap AST

;; Internal SXP representation of CLI objects

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

(defmethod ast ((self cli-ast))
  (cli-ast-ast self))

(defgeneric proc-args (self args))

(defgeneric parse-args (self args &key &allow-other-keys)
  (:documentation "Parse list of strings ARGS using SELF.

A list of the same length as ARGS is returned containing 'cli-ast'
objects: (OPT . (or char string)) (CMD . string) NIL"))
