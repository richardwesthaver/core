;;; krypt/cli.lisp --- Krypt Package CLI

;; 

;;; Code:
(in-package :krypt)

(defcmd b3-cmd ()
  "Call B3SUM on a file or B3HASH-STRING on a string."
  (let ((in (car *args*)))
    (if (probe-file in)
        (println (b3sum in))
        (println (b3hash-string in)))))

(defcmd krypt-show ()
  (init-krypt)
  (println *krypt-user-config*))

(define-cli *krypt-cli*
  :name "krypt"
  :version 0
  :help t
  :description "Crypto Utils"
  :thunk krypt-show
  :opts ((:name "level" :description "set log level" :thunk level-opt)
         (:name "version" :description "print version" :thunk version-opt))
  :cmds ((:name b3 :thunk b3-cmd :description "return a B3 hash or checksum")))
