;;; rls.lisp --- Rustls High-level API

;; 

;;; Code:
(in-package :ssl)

;;; Conditions
(define-condition rls-condition (ssl-condition) ())
(define-condition rls-error (ssl-error) ())

;;; Crypto Provider
(defvar *crypto-provider* nil)

(defun init-rls ()
  "Initialize RLS - ensures rustls shared library is loaded and that the default
crypto provider is initialized for the current process."
  (load-rustls)
  (with-alien ((b (* rustls::rustls-crypto-provider-builder)))
    (rustls::rustls-crypto-provider-builder-new-from-default (addr b))
    (values (setf *crypto-provider* (rustls-crypto-provider-default))
            (rustls:rustls-result* (rustls::rustls-crypto-provider-builder-build-as-default b)))))

;;; Keys
(defun build-rls-certified-key (cert-chain private-key 
                                &optional signing-key (provider (rustls-crypto-provider-default)))
  "Build and return a RUSTLS-CERTIFIED-KEY alien. Typically used to create a
RUSTLS-SERVER-CONFIG and then immediately called with
RUSTLS-CERTIFIED-KEY-FREE. This will transfer ownership of the key to the
config, which will be freed automatically when RUSTLS-SERVER-CONFIG-FREE is
called.

CERT-CHAIN is an octet-vector containing a series of PEM-encoded certs, with
the end-entity (leaf) certificate first.

PRIVATE-KEY is an octet-vector containing a PEM-encoded private key in either
PKCS#1, PKCS#8 or SEC#1 when compiled with default settings (aws-lc-rs as
crypto provider).

Optional SIGNING-KEY is an octet-vector containing the PEM-encoded signing
key, passed to RUSTLS-CRYPTO-PROVIDER-LOAD-KEY using the PROVIDER."
  (let ((cl (length cert-chain)) (pl (length private-key)))
    (with-static-vectors ((c cl :initial-contents cert-chain)
                          (p pl :initial-contents private-key))
      (with-alien ((out (* rustls-certified-key)))
        (if signing-key
            (with-alien ((kout (* rustls-signing-key)))
              (let ((kres (rustls-result* 
                           (rustls-crypto-provider-load-key 
                            provider 
                            (static-vector-pointer p) pl 
                            (addr kout)))))
                (if (eql kres :ok)
                    (values 
                     out
                     (rustls-result* 
                      (rustls-certified-key-build 
                       (static-vector-pointer c) cl 
                       (static-vector-pointer p) pl 
                       (addr out))))
                    (values kout kres))))
            (values 
             out
             (rustls-result* 
              (rustls-certified-key-build 
               (static-vector-pointer c) cl 
               (static-vector-pointer p) pl 
               (addr out)))))))))

;;; Connection
(defstruct rls-connection 
  (sap nil))

(defaccessor sap ((self rls-connection)) (rls-connection-sap self))

;;; Client
(defconfig rls-client-config ()
  ((sap :initform nil :initarg :sap :accessor sap)
   (root-store :initform nil :initarg :root-store :accessor root-store)
   ;; key_log or key_log_file?
   (server-verifier :initarg :server-verifier :accessor server-verifier)
   ;; verifier?
   (enable-sni :initform nil :initarg :enable-sni :accessor enable-sni)
   (certified-keys :initform nil :initarg :certified-keys :accessor certified-keys)))

(defmethod connect ((self rls-client-config) server-name)
  (with-alien ((conn (* rustls::rustls-connection)))
    (values
     conn
     (rustls::rustls-result* (rustls::rustls-client-connection-new (sap self) server-name (addr conn))))))
            
(defmethod build ((self rls-client-config) &key root-store certified-keys)
  (let ((cbuilder (rustls::rustls-client-config-builder-new)))
    (with-alien ((cout (* rustls::rustls-client-config))
                 (ver (* rustls::rustls-server-cert-verifier)))
      (rustls::rustls-result* (rustls::rustls-platform-server-cert-verifier (addr ver)))
      (setf (sap self) cout
            (root-store self) root-store
            (certified-keys self) certified-keys)
      (rustls::rustls-client-config-builder-set-server-verifier cbuilder ver)
      (values 
       self
       (rustls-result* (rustls::rustls-client-config-builder-build cbuilder (addr cout)))))))

;;; Callbacks
(define-alien-callable default-rls-hello-callback (* rustls-certified-key)
    ((userdata rustls-client-hello-userdata)
     (hello (* rustls-client-hello)))
  (with-alien ((out (* rustls-certified-key)))
    (rustls-certified-key-build nil 0 nil 0 (addr out))
    out))

(defun make-rls-hello-callback (fn)
  (declare (ignore fn))
  (alien-callable-function 'default-rls-hello-callback))

(defstruct rls-server-persistence 
  "A struct containing the three required functions for Rustls TLS session ID and
secrets persistence."
  (get (required-argument :get) :type function) 
  (put (required-argument :put) :type function))

;; (defclass rls-session-store () ())

(defun make-rls-session-store-get-callback (fn)
  (declare (ignore fn))
  (alien-callable-function 'default-rls-session-store-get-callback))

(define-alien-callable default-rls-session-store-get-callback unsigned-int
    ((userdata rustls-session-store-userdata)
     (key (* rustls-slice-bytes))
     (remove-after int)
     (buf (* unsigned-char))
     (count size-t)
     (out-n (* size-t)))
  0)

(defun make-rls-session-store-put-callback (fn)
  (declare (ignore fn))
  (alien-callable-function 'default-rls-session-store-put-callback))

(define-alien-callable default-rls-session-store-put-callback unsigned-int
    ((userdata rustls-session-store-userdata)
     (key (* rustls-slice-bytes))
     (val (* rustls-slice-bytes)))
  0)

(defun make-rls-session-store-callbacks (self)
  (values (make-rls-session-store-get-callback (rls-server-persistence-get self))
          (make-rls-session-store-put-callback (rls-server-persistence-put self))))

;;; Server
(defconfig rls-server-config ()
  ((sap :initform nil :initarg :sap :accessor sap)
   (hello :initarg :hello :type function)
   (client-verifier :initarg :client-verifier :initform nil)
   ;; key_log or key_log_file?
   (ignore-client-order :initform nil :type boolean :initarg :ignore-client-order)
   (alpn-protocols :initform nil :type list :initarg :alpn-protocols)
   (certified-keys :initform nil :type list :initarg :certified-keys)
   (persistence :type rls-server-persistence :initarg :persistence
    :documentation "Callbacks for persistence of TLS session IDs and secrets.")))

(defmethod build ((self rls-server-config) &key)
  (let ((cbuilder (rustls::rustls-server-config-builder-new)))
    (with-alien ((cout (* rustls::rustls-server-config)))
      (when (slot-boundp self 'hello)
        (rustls-server-config-builder-set-hello-callback 
         cbuilder 
         (make-rls-hello-callback (slot-value self 'hello))))
      (when (slot-boundp self 'ignore-client-order)
        (rustls-server-config-builder-set-ignore-client-order cbuilder (slot-value self 'ignore-client-order)))
      (when (slot-boundp self 'client-verifier)
        (rustls-server-config-builder-set-client-verifier cbuilder (sap (slot-value self 'client-verifier))))
      (when (slot-boundp self 'persistence)
        (multiple-value-bind (get-cb put-cb) (make-rls-session-store-callbacks (slot-value self 'persistence))
          (rustls-server-config-builder-set-persistence
           cbuilder get-cb put-cb)))
      (setf (sap self) cout)
      (values 
       self
       (rustls-result* (rustls::rustls-server-config-builder-build cbuilder (addr cout)))))))

#|
 * The root cert store can be used in several `rustls_web_pki_client_cert_verifier_builder_new`
 * instances and must be freed by the application when no longer needed. See the documentation of
 * `rustls_root_cert_store_free` for details about lifetime.
|#
;;; Root Cert Store
(defclass rls-root-cert-store () 
  ((certs :initarg :certs :initform nil)
   (strict :initarg :strict :initform nil)))

(defmethod build ((self rls-root-cert-store) &key) ()
  (let ((sbuilder (rustls::rustls-root-cert-store-builder-new)))
    (when-let ((certs (slot-value self 'certs)))
      (let ((strict (slot-value self 'strict)))
        (dolist (c certs)
          (etypecase c
            (pathname (rustls-root-cert-store-builder-load-roots-from-file sbuilder (namestring c) strict))
            (string 
             (rustls-root-cert-store-builder-add-pem 
              sbuilder 
              (octets-to-alien (sb-ext:string-to-octets c)) (length c) 
              strict))
            (octet-vector 
             (rustls-root-cert-store-builder-add-pem 
              sbuilder 
              (octets-to-alien c) (length c) 
              strict))))))
    (sb-alien:with-alien ((sc (* rustls::rustls-root-cert-store)))
      (unwind-protect (values sc (rustls::rustls-root-cert-store-builder-build sbuilder (sb-alien:addr sc)))
        (rustls::rustls-root-cert-store-builder-free sbuilder)))))


;;; Client Cert Verifier
(defclass rls-client-cert-verifier ()
  ((sap :initform nil :initarg :sap :accessor sap)
   (crls :initform nil :initarg :crls)
   (end-entity-only :initform nil :initarg :end-entity-only :type boolean)
   (allow-unknown-revocation-status :initform nil :initarg :allow-unknown-revocation-status :type boolean)
   (allow-unauthenticated :initform nil :initarg :allow-unauthenticated)))

(defclass rls-web-pki-client-cert-verifier (rls-client-cert-verifier) ())

(defmethod build ((self rls-web-pki-client-cert-verifier) &key store) ()
  (let ((builder (rustls::rustls-web-pki-client-cert-verifier-builder-new store)))
    (sb-alien:with-alien ((out (* rustls-web-pki-client-cert-verifier)))
      (setf (sap self) out)
      (unwind-protect (values self (rustls-result*
                                    (rustls::rustls-web-pki-client-cert-verifier-builder-build 
                                     builder
                                     (addr out))))
        (rustls::rustls-web-pki-client-cert-verifier-builder-free builder)))))
