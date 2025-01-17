;;; cli.lisp --- MPK CLI

;; 

;;; Code:
(in-package :mpk/cli)

(defcmd mpk-play-cmd (file)
  (cli/tools))

(define-cli *mpk-cli*
  :name "mpk"
  :help t
  :description "Media Production Kit"
  :cmds ((:name play :thunk mpk-play-cmd)))
