;;; krypt/cli.lisp --- Krypt Package CLI

;; 

;;; Code:
(in-package :krypt)

(define-cli *krypt-cli*
  :help t
  :version 0
  :description "Crypto Utilities"
  :name "krypt")
