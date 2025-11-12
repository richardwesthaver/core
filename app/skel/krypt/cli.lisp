;;; krypt/cli.lisp --- Krypt Package CLI

;; 

;;; Code:
(in-package :skel/krypt)

(defcmd hash-cmd ()
  "Return the CRC64 value of a file or string."
  (let ((in (car *args*)))
    (if (probe-file in)
        (println (crc64-file in))
        (println (crc64-sequence in)))))

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
  :cmds ((:name hash :thunk hash-cmd)))
