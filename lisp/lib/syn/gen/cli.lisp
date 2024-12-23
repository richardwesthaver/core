;;; cli.lisp --- Codegen CLI

;; 

;;; Code:
(in-package :syn/gen/cli)

(define-cli *gen-cli*
  :name "gen"
  :help t
  :version 0
  :description "code generator"
  :opts ((:name "syntax" :kind string :default "c"
          :description "output code syntax")))

(cli:load-package-cli *gen-cli* :package :syn/gen)

(defun run-gen-cli ()
  (with-cli (*gen-cli* :args (args))
    (nyi!)))
