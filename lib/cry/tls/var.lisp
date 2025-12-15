;;; var.lisp --- TLS Variables

;; 

;;; Code:
(in-package :cry/tls)
(defvar *ssl-cipher-list* nil)
(defvar *no-ssl* nil)
(defparameter *ca-bundle*
  #.(namestring #P"/etc/ca-certificates/extracted/ca-bundle.trust.crt")
  "The default public root certificates used for SSL verification.")
(defvar *ssl-buffer-size* 2048
  "The default size of input and output buffers of SSL-STREAM.")
(defvar *ssl-global-context* nil)
(defvar *ssl-global-method* nil)
(defvar *tmp-rsa-key-512* nil)
(defvar *tmp-rsa-key-1024* nil)
(defvar *tmp-rsa-key-2048* nil)
(defvar *ssl-init-lock* (make-mutex :name "ssl-init"))
;; based on http://www.openssl.org/docs/ssl/SSL_CTX_set_default_passwd_cb.html
(defvar *pem-password* ""
  "The callback to be registered with SSL_CTX_set_default_passwd_cb.")
(defvar *ssl-client-stream-verify-default* :required)
