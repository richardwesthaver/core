;;; gen.lisp --- Code Generator

;; CLI access to the SYN/GEN subsystem

;;; Commentary:

;; Takes a file or string and runs is through the appropriate code
;; processor which is inferred from the file type or via cli opt.

;; Outputs generated code to a file specified via cli opt or to
;; *STANDARD-OUTPUT*.

;;; Code:
(defpackage :bin/gen
  (:use :cl :syn/cli :std :cli/clap :log :clap :db :syn/gen))

(in-package :bin/gen)

(defmain start-gen ()
  (with-cli (*gen-cli* :args (cli:args))
    (do-opts *cli*)
    (with-codegen (or *gen* :c)
      (do-cmd *cli*))))
