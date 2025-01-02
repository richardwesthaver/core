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

(defcmd gen-cmd ()
  (println *gen*))

(defcmd gen-print-cmd ()
  (when *args*
    (let ((f (car *args*)))
      (if (probe-file f)
          (print-code (syn/gen/c::read-gen-c-file f))
          (print-code (syn/gen/c::read-gen-c-string f)))
      (terpri))))

(defopt gen-syntax-opt ()
  (let ((syn (keywordicate (string-upcase *arg*))))
    (load-gen syn)
    (setq *package* (gen-package syn))
    syn))

(define-cli *gen-cli*
  :name "gen"
  :description "code generator"
  :version 0
  :help t
  :opts ((:name "output" :kind file)
         (:name "syntax" :kind string :default "c" :thunk gen-syntax-opt)
         (:name "level" :thunk level-opt))
  :cmds ((:name "print" :description "Read GEN-C S expressions from a file or string." :thunk gen-print-cmd))
  :thunk gen-cmd)

(defmain start-gen ()
  (with-cli (*gen-cli* :args (cli:args))
    (do-opts *cli*)
    (do-cmd *cli*)))
