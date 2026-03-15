;;; init.lisp --- SSL Initialization

;; 

;;; Code:
(in-package :cry/tls)

;;; Global state
(defun ssl-initialized-p ()
  (and *ssl-global-context* *ssl-global-method*))

(defun ssl-ctx-set-session-cache-mode (ctx mode)
  (openssl::ssl-ctx-ctrl ctx +SSL-CTRL-SET-SESS-CACHE-MODE+ mode nil))

(defun ssl-set-tlsext-host-name (ctx hostname)
  (openssl::ssl-ctrl ctx 55 #|SSL_CTRL_SET_TLSEXT_HOSTNAME|# 0 #|TLSEXT_NAMETYPE_host_name|# hostname))

(define-alien-callable tmp-rsa-callback (* t) ((ssl (* t)) (export-p int) (key-length int))
  (flet ((rsa-key (length)
           (rsa-generate-key length #.openssl::+RSA-F4+ nil nil)))
    (cond ((= key-length 512)
           (unless *tmp-rsa-key-512*
             (setf *tmp-rsa-key-512* (rsa-key key-length)))
           *tmp-rsa-key-512*)
          ((= key-length 1024)
           (unless *tmp-rsa-key-1024*
             (setf *tmp-rsa-key-1024* (rsa-key key-length)))
           *tmp-rsa-key-1024*)
          (t
           (unless *tmp-rsa-key-2048*
             (setf *tmp-rsa-key-2048* (rsa-key key-length)))
           *tmp-rsa-key-2048*))))

;; based on http://www.openssl.org/docs/ssl/SSL_CTX_set_default_passwd_cb.html
(defvar *pem-password* ""
  "The callback registered with SSL_CTX_set_default_passwd_cb
will use this value.")

;; The callback itself
(define-alien-callable pem-password-callback int
    ((buf (* char)) (size int) (rwflag int) (unused (* t)))
  (let* ((password-str (coerce *pem-password* 'base-string))
         (tmp (make-alien-string password-str)))
    (alien-funcall (extern-alien "strncpy" (function void (* t) (* t) int))
                   buf tmp size)
    (free-alien tmp)
    (setf (deref buf (1- size)) 0)
    (alien-funcall (extern-alien "strlen" (function int (* t))) buf)))

;; The macro to be used by other code to provide password
;; when loading PEM file.
(defmacro with-pem-password ((password) &body body)
  `(let ((*pem-password* (or ,password "")))
     ,@body))

#+nil
(progn
  (defvar *ssl-thread-table* (make-hash-table :weakness :key))
  ;; technically this could overflow, would prefer to auto reset in this case..
  (defvar *ssl-thread-counter* 0)

  (define-alien-callable threadid-callback unsigned-long ()
    (with-recursive-lock (*global-lock*)
      (let ((self (thread-os-tid *current-thread*)))
        (or (gethash self *ssl-thread-table*)
            (setf (gethash self *threads*)
                  (incf *thread-counter*)))))))

(defun ssl-load-global-verify-locations (&rest pathnames)
  "PATHNAMES is a list of pathnames to PEM files containing server and CA certificates.
Install these certificates to use for verifying on all SSL connections.
After RELOAD, you need to call this again."
  (dolist (path pathnames)
    (let ((namestring (namestring (truename path))))
      (with-alien ((cafile c-string namestring))
        (unless (eql 1 (openssl::ssl-ctx-load-verify-locations
                        *ssl-global-context*
                        cafile nil))
          (error "ssl-ctx-load-verify-locations failed."))))))

(defun ssl-set-global-default-verify-paths ()
  "Load the system default verification certificates.
After RELOAD, you need to call this again."
  (unless (eql 1 (ssl-ctx-set-default-verify-paths *ssl-global-context*))
    (error "ssl-ctx-set-default-verify-paths failed.")))

(defmethod init ((self (eql :ssl)) &key method seed)
  (load-ssl)
  (load-crypto)
  (bio-init)
  (when seed 
    (let ((n (length seed)))
      (with-static-vector (v (make-static-vector n :initial-contents seed))
        (openssl::rand-seed (static-vector-pointer v) n))))
  (setf *ssl-global-method* (funcall (or method 'tls-method)))
  (setf *ssl-global-context* (openssl::ssl-ctx-new *ssl-global-method*))
  (unless (eql 1 (openssl::ssl-ctx-set-default-verify-paths *ssl-global-context*))
    (error "ssl-ctx-set-default-verify-paths failed."))
  (ssl-ctx-set-session-cache-mode *ssl-global-context* 3)
  (ssl-set-global-default-verify-paths)
  (openssl::ssl-ctx-set-default-passwd-cb 
   *ssl-global-context*
   (alien-sap (alien-callable-function 'pem-password-callback))))

(defun ensure-ssl (&key method seed)
  (with-recursive-lock (*ssl-init-lock*)
    (unless (ssl-initialized-p)
      (init :ssl :method method :seed seed)))
    (values))
  
(defun use-certificate-chain-file (certificate-chain-file)
  "Apply OpenSSL function SSL_CTX_use_certificate_chain_file
to the global SSL_CTX object and the specified CERTIFICATE-CHAIN-FILE.

OpenSSL requires the certificates in the file to be sorted
starting with the subject's certificate (actual client or
server certificate), followed by intermediate CA certificates
if applicable, and ending at the highest level (root) CA.

Note: the (RESET :SSL) method clears the global context and in particular the
loaded certificate chain."
  (ensure-ssl)
  (openssl::ssl-ctx-use-certificate-chain-file *ssl-global-context* certificate-chain-file))

(defmethod reset ((self (eql :ssl)) &key)
  "If you save your application as a Lisp image, call this function when that
image is loaded, to perform the necessary SSL re-initialization (unless your
lisp implementation automatically re-loads foreign libraries and preserves
their memory accross image reloads)."
  (unless (member :crypto *features*)
    (load-crypto))
  (unless (member :ssl *features*)
    (load-ssl))
  (setf *ssl-global-context* nil
        *ssl-global-method* nil
        *tmp-rsa-key-512* nil
        *tmp-rsa-key-1024* nil))

