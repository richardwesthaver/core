;;; condition.lisp --- SSL Errors

;; 

;;; Code:
(in-package :cry/tls)

(define-condition ssl-error (openssl::openssl-sap-error)
  ((;; When SSL_Connect or SSL_Accept fail due to
    ;; the SSL_VERIFY_PEER flag and bad peer certificate,
    ;; the error queue simply says "certificate verify failed"
    ;; and the user needs to call SSL_get_verify_result
    ;; to find our the exact verification error (expired cert,
    ;; can't get issuer cert locally, etc).
    ;;
    ;; To facilitate debugging and logging, we
    ;; automaticall store the SSL_get_verify_result
    ;; in this slot and use it in the printed
    ;; representation of the condition.
    ;;
    ;; Ideally, we should only collect the verification
    ;; error if the error queue includes reason code
    ;; SSL_R_CERTIFICATE_VERIFY_FAILED for library
    ;; code ERR_LIB_SSL, but this would require
    ;; us to implement the logic of OpenSSL macros
    ;; ERR_raise, ERR_PACK, taking OpenSSL version into
    ;; account - those macros produce different number
    ;; for that reason code in different OpenSSL versions.
    ;; Here are snippets of printed error queues, starting
    ;; with error code:
    ;;   openssl-0.9.8zh
    ;;     14090086:SSL routines:SSL3_GET_SERVER_CERTIFICATE:certificate verify failed:s3_clnt.c:973:
    ;;   openssl-1.1.1p
    ;;     1416F086:SSL routines:tls_process_server_certificate:certificate verify failed:ssl/statem/statem_clnt.c:1919:
    ;;   openssl-3.0.4
    ;;     0A000086:SSL routines:tls_post_process_server_certificate:certificate verify failed:ssl/statem/statem_clnt.c:1887:
    ;; Therefore we simply collect the verification
    ;; error if it is present at the time of SSL_Connect
    ;; or SSL_Accept failure - see how the
    ;; collecting-verify-error macro is used.
    ;; This approach, however, will not collect verification
    ;; error if it happens not on the initial handshake,
    ;; but during session renegotiation.
    verify-error :type (or null string)
                 :initform nil
                 :accessor ssl-error-verify-error))
  (:documentation
   "A failure in the SSL library occurred, usually a protocol error. The
    OpenSSL error queue contains more information on the error.")
  (:report (lambda (condition stream)
             (format stream
                     "A failure in the SSL library occurred on handle ~A (SSL_get_error: ~A). "
                     (openssl::error-sap condition)
                     (openssl::error-code condition))
             (openssl::format-error-queue stream condition)
             (when (ssl-error-verify-error condition)
               (format stream
                       "~A"
                       (ssl-error-verify-error condition))))))

(defun collect-verify-error (ssl-error-ssl-condition handle)
  (let ((code (openssl::ssl-get-verify-result handle)))
    (unless (eql code openssl::+x509-v-ok+)
      (setf (ssl-error-verify-error ssl-error-ssl-condition)
            (format nil "SSL_get_verify_result: ~d~@[ ~a~]"
                    code (openssl::openssl-verify-error-keyword code))))))

(defun collecting-verify-error-impl (handle body-fn)
  (handler-bind ((ssl-error (lambda (c)
                              (collect-verify-error c handle))))
    (funcall body-fn)))

(defmacro collecting-verify-error ((handle) &body body)
  `(collecting-verify-error-impl ,handle (lambda () ,@body)))

(defun err-print-errors-to-string ()
  (with-bio-output-to-string (bio)
    (openssl::err-print-errors bio)))

(defun ssl-signal-error (handle syscall error-code ret)
  "RET is return value of the failed SYSCALL (like SSL_read, SSL_connect,
SSL_shutdown, etc - most of them designate failure by returning
RET <= 0, althought SSL_shutdow fails with RET < 0.

ERROR-CODE is return value of SSL_get_error - an explanation of the failure."
  (let ((printed-queue (err-print-errors-to-string)))
    (if (and (eql error-code #.openssl::+ssl-error-syscall+)
             (not (zerop ret)))
        (error 'openssl-error-syscall
               :sap handle
               :code error-code
               :queue printed-queue
               :syscall syscall)
        (error (case error-code
                 (#.+ssl-error-none+ 'openssl-error-none)
                 (#.+ssl-error-ssl+ 'ssl-error)
                 (#.+ssl-error-want-read+ 'openssl-error-want-read)
                 (#.+ssl-error-want-write+ 'openssl-error-want-write)
                 (#.+ssl-error-want-x509-lookup+ 'openssl-error-want-x509-lookup)
                 (#.+ssl-error-syscall+ 'openssl-error-zero-return) ; this is intentional here. we got an EOF from the syscall (ret is 0)
                 (#.+ssl-error-zero-return+ 'openssl-error-zero-return)
                 (#.+ssl-error-want-connect+ 'openssl-error-want-connect)
                 (t 'openssl-sap-error))
               :sap handle
               :code error-code
               :queue printed-queue))))
