;;; krypt/cli.lisp --- Krypt Package CLI

;; 

;;; Code:
(in-package :krypt)

(defcmd b3-cmd ()
  (let ((in (car *args*)))
    (if (probe-file in)
        (b3sum in)
        (b3hash-string in))))
