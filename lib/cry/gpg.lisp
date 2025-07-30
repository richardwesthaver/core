;;; gpg.lisp --- GnuPG

;; 

;;; Code:
(in-package :cry/gpg)
(defvar *user-gpg-directory* (merge-homedir-pathnames ".gnupg/"))

(definline user-gpg-config-file () (probe-file (merge-pathnames *user-gpg-directory* "gpg.conf")))
(definline user-gpg-agent-config-file () (probe-file (merge-pathnames *user-gpg-directory* "gpg-agent.conf")))

(defun load-gpg-config-file (path)
  (with-open-file (f path)
    (loop for c = (peek-char t f nil nil)
          when (null c) do (loop-finish)
          if (char= c #\#) do (read-line f nil)
          if (whitespace-p c) do (read-char f nil)
          else collect (read-line f nil))))

(defconfig gpg-config (ast) ())

(defconfig gpg-agent-config (gpg-config) ())

(defmethods make-config
  (((self (eql :gpg)) &key (path (user-gpg-config-file)))
   (when path
     (make-instance 'gpg-config :ast (load-gpg-config-file path))))
  (((self (eql :gpg-agent)) &key (path (user-gpg-agent-config-file)))
   (when path
     (make-instance 'gpg-agent-config :ast (load-gpg-config-file path)))))
