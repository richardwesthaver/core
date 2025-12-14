;;; condition.lisp --- OPENSSL Conditions

;; 

;;; Code:
(in-package :openssl)

(defconstant +ssl-error-none+ 0)
(defconstant +ssl-error-ssl+ 1)
(defconstant +ssl-error-want-read+ 2)
(defconstant +ssl-error-want-write+ 3)
(defconstant +ssl-error-want-x509-lookup+ 4)
(defconstant +ssl-error-syscall+ 5)
(defconstant +ssl-error-zero-return+ 6)
(defconstant +ssl-error-want-connect+ 7)

(define-alien-enum (err-r)
  :sys-lib (err-lib :sys)
  :bn-lib (err-lib :bn)
  :rsa-lib (err-lib :rsa)
  :dh-lib (err-lib :dh)
  :evp-lib (err-lib :evp)
  :buf-lib (err-lib :buf)
  :obj-lib (err-lib :obj)
  :pem-lib (err-lib :pem)
  :dsa-lib (err-lib :dsa)
  :x509-lib (err-lib :x509)
  :asn1-lib (err-lib :asn1)
  :conf-lib (err-lib :conf)
  :crypto-lib (err-lib :crypto)
  :ec-lib (err-lib :ec)
  :ssl-lib (err-lib :ssl)
  :bio-lib (err-lib :bio)
  :pkcs7-lib (err-lib :pkcs7)
  :pkcs8-lib (err-lib :pkcs8)
  :x509v3-lib (err-lib :x509v3)
  :rand-lib (err-lib :rand)
  :dso-lib (err-lib :dso)
  :engine-lib (err-lib :engine)
  :ocsp-lib (err-lib :ocsp)
  :ui-lib (err-lib :ui)
  :comp-lib (err-lib :comp)
  :ecdsa-lib (err-lib :ecdsa)
  :ecdh-lib (err-lib :ecdh)
  ;; todo: where is this defined?
  ;;  :store-lib (err-lib :store)
  :fips-lib (err-lib :fips)
  :cms-lib (err-lib :cms)
  :ts-lib (err-lib :ts)
  :hmac-lib (err-lib :hmac)
  ;; TODO: where is this defined?
  ;; :jpake-lib (err-lib :jpake)
  :user-lib (err-lib :user)
  :digest-lib (err-lib :digest)
  :cipher-lib (err-lib :cipher)
  :hkdf-lib (err-lib :hkdf)
  :trust-token-lib (err-lib :trust-token)
  :fatal 64
  :malloc-failure (logior 1 64)
  :should-not-have-been-called (logior 2 64)
  :passed-null-parameter (logior 3 64)
  :internal-error (logior 4 64)
  :overflow (logior 5 64))

(define-condition openssl-condition () ())

(define-condition openssl-error (openssl-condition error) 
  ((queue :initform nil :initarg :queue :reader error-queue)))

(define-condition openssl-sap-error (openssl-error)
  ((code :initarg :code
         :reader error-code
         :documentation "The error code returned by SSL_get_error. " )
   (sap :initarg :sap
        :reader error-sap))
  (:documentation "Base condition for lisp wrappers of SSL_get_error return values.")
  (:report (lambda (condition stream)
             (format stream "Unspecified error ~A on handle ~A. "
                     (error-code condition)
                     (error-sap condition))
             (format-error-queue stream condition))))

(define-condition openssl-error-initialize (openssl-error)
  ((reason  :initarg :reason
            :reader error-reason))
  (:report (lambda (condition stream)
             (format stream "SSL initialization error: ~A. "
                     (error-reason condition))
             (format-error-queue stream condition))))

(define-condition openssl-error-want-something (openssl-sap-error)
  ())

;; SSL_ERROR_NONE
(define-condition openssl-error-none (openssl-sap-error)
  ()
  (:documentation
   "The TLS/SSL I/O operation completed. This result code is returned if and
    only if ret > 0.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL operation on handle ~A completed (SSL_get_error: ~A). "
                     (error-sap condition)
                     (error-code condition))
             (format-error-queue stream condition))))

;; SSL_ERROR_ZERO_RETURN
(define-condition openssl-error-zero-return (openssl-sap-error)
  ()
  (:documentation
   "The TLS/SSL connection has been closed. If the protocol version is SSL 3.0
    or TLS 1.0, this result code is returned only if a closure alert has
    occurred in the protocol, i.e. if the connection has been closed cleanly.
    Note that in this case SSL_ERROR_ZERO_RETURN
    does not necessarily indicate that the underlying transport has been
    closed.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL connection on handle ~A has been closed (SSL_get_error: ~A). "
                     (error-sap condition)
                     (error-code condition))
             (format-error-queue stream condition))))

;; SSL_ERROR_WANT_READ
(define-condition openssl-error-want-read (openssl-error-want-something)
  ()
  (:documentation
   "The operation did not complete; the same TLS/SSL I/O function should be
    called again later. If, by then, the underlying BIO has data available for
    reading (if the result code is SSL_ERROR_WANT_READ) or allows writing data
    (SSL_ERROR_WANT_WRITE), then some TLS/SSL protocol progress will take place,
    i.e. at least part of an TLS/SSL record will be read or written. Note that
    the retry may again lead to a SSL_ERROR_WANT_READ or SSL_ERROR_WANT_WRITE
    condition. There is no fixed upper limit for the number of iterations that
    may be necessary until progress becomes visible at application protocol
    level.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL operation on handle ~A did not complete: It wants a READ (SSL_get_error: ~A). "
                     (error-sap condition)
                     (error-code condition))
             (format-error-queue stream condition))))

;; SSL_ERROR_WANT_WRITE
(define-condition openssl-error-want-write (openssl-error-want-something)
  ()
  (:documentation
   "The operation did not complete; the same TLS/SSL I/O function should be
    called again later. If, by then, the underlying BIO has data available for
    reading (if the result code is SSL_ERROR_WANT_READ) or allows writing data
    (SSL_ERROR_WANT_WRITE), then some TLS/SSL protocol progress will take place,
    i.e. at least part of an TLS/SSL record will be read or written. Note that
    the retry may again lead to a SSL_ERROR_WANT_READ or SSL_ERROR_WANT_WRITE
    condition. There is no fixed upper limit for the number of iterations that
    may be necessary until progress becomes visible at application protocol
    level.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL operation on handle ~A did not complete: It wants a WRITE (SSL_get_error: ~A). "
                     (error-sap condition)
                     (error-code condition))
             (format-error-queue stream condition))))

;; SSL_ERROR_WANT_CONNECT
(define-condition openssl-error-want-connect (openssl-error-want-something)
  ()
  (:documentation
   "The operation did not complete; the same TLS/SSL I/O function should be
    called again later. The underlying BIO was not connected yet to the peer
    and the call would block in connect()/accept(). The SSL
    function should be called again when the connection is established. These
    messages can only appear with a BIO_s_connect() or
    BIO_s_accept() BIO, respectively. In order to find out, when
    the connection has been successfully established, on many platforms
    select() or poll() for writing on the socket file
    descriptor can be used.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL operation on handle ~A did not complete: It wants a connect first (SSL_get_error: ~A). "
                     (error-sap condition)
                     (error-code condition))
             (format-error-queue stream condition))))

;; SSL_ERROR_WANT_X509_LOOKUP
(define-condition openssl-error-want-x509-lookup (openssl-error-want-something)
  ()
  (:documentation
   "The operation did not complete because an application callback set by
    SSL_CTX_set_client_cert_cb() has asked to be called again. The
    TLS/SSL I/O function should be called again later. Details depend on the
    application.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL operation on handle ~A did not complete: An application callback wants to be called again (SSL_get_error: ~A). "
                     (error-sap condition)
                     (error-code condition))
             (format-error-queue stream condition))))

;; SSL_ERROR_SYSCALL
(define-condition openssl-error-syscall (openssl-sap-error)
  ((syscall :initarg :syscall))
  (:documentation
   "Some I/O error occurred. The OpenSSL error queue may contain more
    information on the error. If the error queue is empty (i.e. ERR_get_error() returns 0),
    ret can be used to find out more about the error: If ret == 0, an EOF was observed that
    violates the protocol. If ret == -1, the underlying BIO reported an I/O error (for socket
    I/O on Unix systems, consult errno for details).")
  (:report (lambda (condition stream)
             (case (error-code condition)
               (0 (format stream "An I/O error occurred: An unexpected EOF was observed on handle ~A (SSL_get_error: ~A). "
                          (error-sap condition)
                          (error-code condition)))
               (-1 (format stream "An I/O error occurred in the underlying BIO (SSL_get_error: ~A). "
                           (error-code condition)))
               (otherwise (format stream "An I/O error occurred: undocumented reason (SSL_get_error: ~A). "
                                  (error-code condition))))
             (format-error-queue stream condition))))

(defparameter *ssl-verify-error-alist*
  '((0 :X509_V_OK)
    (2 :X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT)
    (3 :X509_V_ERR_UNABLE_TO_GET_CRL)
    (4 :X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE)
    (5 :X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE)
    (6 :X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY)
    (7 :X509_V_ERR_CERT_SIGNATURE_FAILURE)
    (8 :X509_V_ERR_CRL_SIGNATURE_FAILURE)
    (9 :X509_V_ERR_CERT_NOT_YET_VALID)
    (10 :X509_V_ERR_CERT_HAS_EXPIRED)
    (11 :X509_V_ERR_CRL_NOT_YET_VALID)
    (12 :X509_V_ERR_CRL_HAS_EXPIRED)
    (13 :X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD)
    (14 :X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD)
    (15 :X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD)
    (16 :X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD)
    (17 :X509_V_ERR_OUT_OF_MEM)
    (18 :X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT)
    (19 :X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN)
    (20 :X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY)
    (21 :X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE)
    (22 :X509_V_ERR_CERT_CHAIN_TOO_LONG)
    (23 :X509_V_ERR_CERT_REVOKED)
    (24 :X509_V_ERR_INVALID_CA)
    (25 :X509_V_ERR_PATH_LENGTH_EXCEEDED)
    (26 :X509_V_ERR_INVALID_PURPOSE)
    (27 :X509_V_ERR_CERT_UNTRUSTED)
    (28 :X509_V_ERR_CERT_REJECTED)
    (29 :X509_V_ERR_SUBJECT_ISSUER_MISMATCH)
    (30 :X509_V_ERR_AKID_SKID_MISMATCH)
    (31 :X509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH)
    (32 :X509_V_ERR_KEYUSAGE_NO_CERTSIGN)
    (50 :X509_V_ERR_APPLICATION_VERIFICATION)))

(defun openssl-verify-error-keyword (code)
  (cadr (assoc code *ssl-verify-error-alist*)))

(defun openssl-verify-error-code (keyword)
  (caar (member keyword *ssl-verify-error-alist* :key #'cadr)))

(define-condition openssl-error-verify (openssl-error)
  ((stream :initarg :stream
           :reader error-stream
           :documentation "The SSL stream whose peer certificate didn't verify.")
   (code :initarg :code
         :reader error-code
         :documentation "The peer certificate verification error code
(as returned by functions like SSL_get_verify_result or X509_STORE_CTX_get_error)."))
  (:report (lambda (condition stream)
             (let ((code (error-code condition)))
               (format stream "SSL verify error: ~d~@[ ~a~]"
                       code (openssl-verify-error-keyword code)))))
  (:documentation "This condition is signalled on SSL connection when a peer certificate doesn't verify."))

(define-condition openssl-error-call (openssl-error std:std-error)
  ()
  (:documentation
   "A failure in the SSL library occurred..")
  (:report (lambda (condition stream)
             (format stream "A failure in OpenSSL library occurred~@[: ~A~]. "
                     (std:error-message condition))
             (format-error-queue stream condition))))

(defun read-openssl-error-queue ()
  (loop for error-code = (err-get-error)
        until (zerop error-code)
        collect error-code))

(defun format-error-queue (stream-designator queue-designator)
  "STREAM-DESIGNATOR is the same as CL:FORMAT accepts: T, NIL, or a stream.
QUEUE-DESIGNATOR is either a list of error codes (as returned
by READ-SSL-ERROR-QUEUE) or an SSL-ERROR condition."
  (flet ((body (stream)
           (let ((queue (etypecase queue-designator
                          (openssl-error (error-queue queue-designator))
                          (string queue-designator))))
             (format stream "~&SSL error queue")
             (if (not (sequence:emptyp queue))
                 (format stream ":~%~A~%" queue)
                 (format stream " is empty.")))))
    (case stream-designator
      ((t) (body *standard-output*))
      ((nil) (let ((s (make-string-output-stream :element-type 'character)))
               (unwind-protect
                    (body s)
                 (close s))
               (get-output-stream-string s)))
      (t (body stream-designator)))))

(define-condition asn1-error (openssl-condition error)
  ()
  (:documentation "Asn1 syntax error"))

(define-condition invalid-asn1-string (openssl-condition error)
  ((type :initarg :type :initform nil))
  (:documentation "ASN.1 string parsing/validation error")
  (:report (lambda (condition stream)
             ;; TODO: when moved to grovel use enum symbol here
             (format stream "ASN.1 syntax error: invalid asn1 string (expected type ~a)" (slot-value condition 'type)))))

(define-condition server-certificate-missing (openssl-condition simple-error)
  ()
  (:documentation "SSL server didn't present a certificate"))
