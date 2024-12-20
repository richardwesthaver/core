;;; cli.lisp --- MPK CLI

;; 

;;; Code:
(in-package :mpk/cli)

(define-cli *mpk-cli*
  :name "mpk"
  :help t
  :description "Media Production Kit")
