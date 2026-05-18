;;; var.lisp --- Krypt Variables

;; 

;;; Code:
(in-package :krypt)
(defvar *user-kryptrc* (xdg-config-file :krypt))
(defvar *krypt-directory* (xdg-data-directory "krypt"))
(defvar *krypt-key-directory* (merge-pathnames "key/" *krypt-directory*))
(defvar *krypt-token-directory* (merge-pathnames "token/" *krypt-directory*))
(defvar *krypt-password-directory* (merge-pathnames "pw/" *krypt-directory*))
(defvar *krypt-net-directory* (merge-pathnames "net/" *krypt-directory*))
(defvar *krypt-user-config* nil)
