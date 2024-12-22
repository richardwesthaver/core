;;; gen.lisp --- Code Generator

;; CLI access to the SYN/GEN subsystem

;;; Commentary:

;; Takes a file or string and runs is through the appropriate code
;; processor which is inferred from the file type or via cli opt.

;; Outputs generated code to a file specified via cli opt or to
;; *STANDARD-OUTPUT*.

;;; Code:
(defpackage :bin/gen
  (:use :cl :syn/gen :std :cli/clap :log :clap :db))

(in-package :bin/gen)

(define-cli *gen-cli*
  :name "gen"
  :description "code generator"
  :version 0
  :help t
  :opts ((:name "output")
         (:name "syntax")
         (:name "level")))

(defmain start-gen ()
  (with-cli (*gen-cli* :args (cli:args) :run t)))

