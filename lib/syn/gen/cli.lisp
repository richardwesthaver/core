;;; cli.lisp --- Codegen CLI

;; 

;;; Code:
(in-package :syn/gen/cli)
(defcmd gen-cmd () (println syn/gen:*gen*))

(defcmd gen-print-cmd ()
  (when *args*
    (let ((f (car *args*)))
      (if (probe-file f)
	  (print-code (syn/gen/c::read-gen-c-file f))
	  (print-code (syn/gen/c::read-gen-c-string f)))
      (terpri))))

(defopt gen-syntax-opt ()
  (let ((syn (keywordicate (string-upcase *arg*))))
    (syn/gen:load-gen syn)
    (setq *package* (syn/gen:gen-package syn))
    syn))

(define-cli *gen-cli*
  :name "gen"
  :package :syn/gen
  :description "code generator"
  :version 0
  :help t
  :opts ((:name "output" :kind file)
	 (:name "syntax" :kind string :default "c" :thunk gen-syntax-opt)
	 (:name "level" :thunk level-opt))
  :cmds ((:name "print" :description "Read GEN-C S expressions from a file or string." :thunk gen-print-cmd))
  :thunk gen-cmd)

(cli:load-package-cli *gen-cli* :package :syn/gen)

(defun run-gen-cli ()
  (with-cli (*gen-cli* :args (args))
    (nyi!)))
