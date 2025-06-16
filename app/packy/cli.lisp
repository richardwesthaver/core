;;; cli.lisp --- Packy CLI Defs

;; 

;;; Code:
(in-package :packy/cli)

(define-cli *packy-cli*
  :help t
  :name "packy"
  :version "0.1.0"
  :description "Universal Package Manager"
  :thunk pk-show
  :opts ((:name "level" :description "set the log level" :thunk level-opt)
         (:name "version" :description "print version" :thunk version-opt))
  :cmds ((:name show
          :opts ((:name "target" :thunk pk-target))
          :thunk pk-show)))
