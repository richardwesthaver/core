;;; cli.lisp --- VC CLI Implementation

;; 

;;; Code:
(in-package :vc/cli)

(define-cli *vc-cli*
  :name "vc"
  :help t
  :version 0
  :description "Version Controller")
