;;; condition.lisp --- AWS-LC Conditions

;; 

;;; Code:
(in-package :aws-lc)

(define-alien-enum (err-r int)
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

(define-condition aws-lc-condition () ())

(define-condition aws-lc-error (aws-lc-condition error) 
  ((queue :initform nil :initarg :queue :reader error-queue)))

(define-condition aws-lc-error-call (aws-lc-error std:std-error)
  ()
  (:documentation
   "A failure in the SSL library occurred..")
  (:report (lambda (condition stream)
             (format stream "A failure in OpenSSL library occurred~@[: ~A~]. "
                     (std:error-message condition))
             (format-error-queue stream condition))))

(defun read-aws-lc-error-queue ()
  (loop for error-code = (err-get-error)
        until (zerop error-code)
        collect error-code))

(defun format-error-queue (stream-designator queue-designator)
  "STREAM-DESIGNATOR is the same as CL:FORMAT accepts: T, NIL, or a stream.
QUEUE-DESIGNATOR is either a list of error codes (as returned
by READ-SSL-ERROR-QUEUE) or an SSL-ERROR condition."
  (flet ((body (stream)
           (let ((queue (etypecase queue-designator
                          (aws-lc-error (error-queue queue-designator))
                          (list queue-designator))))
             (format stream "SSL error queue")
             (if queue
                 (progn
                   (format stream ":~%")
                   (loop for error-code in queue
                         do (format stream "~a~%" (err-error-string error-code nil))))
                 (format stream " is empty.")))))
    (case stream-designator
      ((t) (body *standard-output*))
      ((nil) (let ((s (make-string-output-stream :element-type 'character)))
               (unwind-protect
                    (body s)
                 (close s))
               (get-output-stream-string s)))
      (t (body stream-designator)))))
