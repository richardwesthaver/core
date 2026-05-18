;;; util.lisp --- Krypt Utils

;;

;;; Code:
(in-package :krypt)

(defun init-krypt ()
  "Initialize the global KRYPT environment:

*KRYPT-USER-CONFIG*"
  (setq *user-kryptrc* (xdg-config-file "kryptrc"))
  (mapc 'ensure-directories-exist
        (list *krypt-directory* *krypt-net-directory*
              *krypt-token-directory* *krypt-password-directory*))
  (setq *krypt-user-config* (load-kryptrc)))

(defmethod init ((self (eql :krypt)) &key)
  (init :xdg)
  (init-crc64 +improved-polynomial+)
  (init-krypt))
