;;; krypt/cli.lisp --- Krypt Package CLI

;; 

;;; Code:
(in-package :skel/krypt)

(defcommand (:krypt hash) (input)
  "Return the CRC64 value of a file or string."
  (if (probe-file input)
      (println (crc64-file input))
      (println (crc64-sequence input))))

(defcommand (:krypt show) ()
  (init-krypt)
  (println *krypt-user-config*))

#+todo
(define-cli "krypt"
  :description "Crypto Utils")
