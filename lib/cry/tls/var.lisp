;;; var.lisp --- TLS Variables

;; 

;;; Code:
(in-package :cry/tls)
(defvar *ssl-buffer-size* 2048
  "The default size of input and output buffers of SSL-STREAM.")
(defvar *ssl-context* nil)
(defvar *ssl-method* nil)
(defvar *rsa-key-512* nil)
(defvar *rsa-key-1024* nil)
(defvar *rsa-key-2048* nil)
(defvar *ssl-init-lock* (make-mutex :name "ssl-init"))
(defvar *ssl-locks* nil)
;; based on http://www.openssl.org/docs/ssl/SSL_CTX_set_default_passwd_cb.html
(defvar *pem-password* ""
  "The callback to be registered with SSL_CTX_set_default_passwd_cb.")

(defmethod init ((self (eql :ssl)) &key method rand-seed))
