;;; cli.lisp --- Organ CLI

;; 

;;; Code:
(in-package :organ/cli)

(define-cli *organ-cli*
  :name "organ"
  :help t
  :version 0
  :description "Org tools")

(cli:load-package-cli *organ-cli* :package :organ)

(defun run-gen-cli ()
  (with-cli (*organ-cli* :args (args))
    (nyi!)))
