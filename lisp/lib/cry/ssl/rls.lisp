;;; rls.lisp --- Rustls High-level API

;; 

;;; Code:
(in-package :ssl)

(defstruct rls-connection (sap nil))
(defaccessor (sap) ((self rls-connection)) (rls-connection-sap self))

(defclass rls-client-config (config) 
  ((sap :initform nil :initarg :sap :accessor sap)
   (root-store :initform nil :initarg :root-store :accessor root-store)
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
       (rustls::rustls-client-config-builder-build cbuilder (addr cout))))))

(defclass rls-server-config (config) ())
(defclass rls-session-store () ())

(defun build-root-store ()
  (let ((sbuilder (rustls::rustls-root-cert-store-builder-new)))
    (sb-alien:with-alien ((sc (* rustls::rustls-root-cert-store)))
      (unwind-protect (values sc (rustls::rustls-root-cert-store-builder-build sbuilder (sb-alien:addr sc)))
        (rustls::rustls-root-cert-store-builder-free sbuilder)))))
