;;; x509.lisp --- X.509 Coding

;; ASN.1-based standard defining the format of public key certificates.

;;; Commentary:

;; X.509 certs are used in many internet protocols including TLS/SSL as well
;; as in offline applications as electornic signatures.

;; Certs bind an identity to a public key using a dignital signature.

;; X.509 also defines certificate revocation lists (CRLs) as well as a
;; certification path validation algorithm which allows certs to be signed by
;; chains of intermediates, eventually reaching a trust anchor.

;; rfc5280

;;; Code:
(defpackage :tls/x509
  (:nicknames :x509)
  (:use :cl :std :openssl :dat/asn1 :sb-alien)
  (:export
   #:decode-der-octet-vector
   #:decode-certificate-from-file))

(in-package :tls/x509)

(defun decode-der-octet-vector (bytes)
  (with-vector-sap (buffer bytes)
    (with-alien ((buf (* unsigned-char) (sap-alien buffer (* unsigned-char))))
      (let ((cert (d2i-x509 nil (addr buf) (length bytes))))
        (when (null-alien cert)
          (error 'aws-lc-error-call :message "d2i-X509 failed" :queue (read-aws-lc-error-queue)))
        cert))))

(defun decode-pem-octet-vector (bytes)
  (with-vector-sap (buffer bytes)
    (with-alien ((buf (* unsigned-char) (sap-alien buffer (* unsigned-char))))
      (let ((cert (d2i-x509 nil (addr buf) (length bytes))))
        (when (null-alien cert)
          (error 'aws-lc-error-call :message "d2i-X509 failed" :queue (read-aws-lc-error-queue)))
        cert))))

(defun cert-format-from-path (path)
  "Return the assumed format of PATH - :DER if it is specified as the extension
else defaults to :PEM."
  (if (string-equal "der" (pathname-type path))
      :der
      :pem))

(defun slurp-stream (stream)
  "Returns a sequence containing the STREAM bytes; the
sequence is created by IO/STATIC:MAKE-STATIC-VECTOR.
therefore it can safely be passed to
 STD:WITH-VECTOR-SAP."
  (let ((seq (io/static:make-static-vector (file-length stream))))
    (read-sequence seq stream)
    seq))

(defun decode-certificate-from-file (path &key format)
  "Decode a X.509 certificate from PATH. The FORMAT is interpreted from the path
extension when NIL (:DER or :PEM)."
  (let ((bytes (with-open-file (stream path :element-type '(unsigned-byte 8))
                 (slurp-stream stream)))
        (format (or format (cert-format-from-path path))))
    (case format
      (:der (decode-der-octet-vector bytes))
      (:pem (decode-pem-octet-vector bytes)))))
