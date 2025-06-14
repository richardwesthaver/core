;;; rls.lisp --- Rustls High-level API

;; 

;;; Code:
(in-package :ssl)

(define-condition rls-condition (ssl-condition) ())
(define-condition rls-error (ssl-error) ())

(defstruct rls-connection 
  (sap nil))

(defaccessor sap ((self rls-connection)) (rls-connection-sap self))

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
