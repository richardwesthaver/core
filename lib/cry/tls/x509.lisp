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
(in-package :cry/tls)

(defun decode-der-octet-vector (bytes)
  (with-vector-sap (buffer bytes)
    (with-alien ((buf (* unsigned-char) (sap-alien buffer (* unsigned-char))))
      (let ((cert (d2i-x509 nil (addr buf) (length bytes))))
        (when (null-alien cert)
          (error 'aws-lc-error-call :message "d2i-X509 failed" :queue (read-openssl-error-queue)))
        cert))))

(defun decode-pem-octet-vector (bytes)
  (with-vector-sap (buffer bytes)
    (with-alien ((buf (* unsigned-char) (sap-alien buffer (* unsigned-char))))
      (let ((cert (d2i-x509 nil (addr buf) (length bytes))))
        (when (null-alien cert)
          (error 'aws-lc-error-call :message "d2i-X509 failed" :queue (read-openssl-error-queue)))
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

(defun certificate-alt-names (cert)
  #|
  * The return value is the decoded extension or NULL on
  * error. The actual error can have several different causes,
  * the value of *crit reflects the cause:
  * >= 0, extension found but not decoded (reflects critical value).
  * -1 extension not found.
  * -2 extension occurs more than once.
  |#
  (with-alien ((crit int))
    (let ((result (x509-get-ext-d2i cert +NID-subject-alt-name+ (addr crit) nil)))
      (if (null-alien result)
          (cond
              ((>= crit 0)
               (error "X509_get_ext_d2i: subject-alt-name extension decoding error"))
              ((= crit -1) ;; extension not found, return NULL
               result)
              ((= crit -2)
               (error "X509_get_ext_d2i: subject-alt-name extension occurs more than once"))))
          result)))

(defun certificate-dns-alt-names (cert)
  (let ((altnames (certificate-alt-names cert)))
    (unless (or (null altnames) (null-alien altnames))
      (unwind-protect
          (flet ((alt-name-to-string (alt-name)
                   (let ((name (cast alt-name (* openssl::general-name))))
                     (with-alien-slots ((type openssl::type) (data openssl::data)) name
                       (case type
                         (#.openssl::+GEN-IPADD+
                          (let ((address (dat/asn1::asn1-string-octet-vector data)))
                            (sb-bsd-sockets:host-ent-name (sb-bsd-sockets:get-host-by-address address))))
                         (#.openssl::+GEN-DNS+
                          (or (try-get-asn1-string-data data '(#.(v-asn1 :ia5string)))
                              (error "Malformed certificate: possibly NULL in dns-alt-name"))))))))
            (let ((altnames-count (openssl::openssl-sk-num altnames)))
              (loop for i from 0 below altnames-count
                    as alt-name = (openssl::openssl-sk-value altnames i)
                    collect (alt-name-to-string alt-name))))
        (general-names-free altnames)))))

(defun certificate-subject-common-names (cert)
  (let ((i -1)
        (subject-name (x509-get-subject-name cert)))
    (when (or (null subject-name) (null-alien subject-name))
      (error "X509_get_subject_name returned NULL"))
    (flet ((extract-cn ()
             (setf i (x509-name-get-index-by-nid subject-name +NID-commonName+ i))
             (when (>= i 0)
               (let* ((entry (x509-name-get-entry subject-name i)))
                 (when (null-alien entry)
                   (error "X509_NAME_get_entry returned NULL"))
                 (let ((entry-data (x509-name-entry-get-data entry)))
                   (when (null-alien entry-data)
                     (error "X509_NAME_ENTRY_get_data returned NULL"))
                   (try-get-asn1-string-data entry-data '(#.openssl::+v-asn1-utf8string+
                                                          #.openssl::+v-asn1-bmpstring+
                                                          #.openssl::+v-asn1-printablestring+
                                                          #.openssl::+v-asn1-universalstring+
                                                          #.openssl::+v-asn1-teletexstring+)))))))
      (loop
        as cn = (extract-cn)
        if cn collect cn
        if (not cn) do
           (loop-finish)))))

(defun certificate-not-after-time (certificate)
  "Returns a universal-time representing the time after
which the CERTIFICATE is not valid. Signals an ERROR if the
CERTIFICATE does not have a properly formatted time. "
  (let ((asn1-time (x509-get0-not-after certificate)))
    (when (null-alien asn1-time)
      (error "X509_get0_notAfter returned NULL"))
    (decode-asn1-time asn1-time)))

(defun certificate-not-before-time (certificate)
  "Returns a universal-time representing the time before
which the CERTIFICATE is not valid. Signals an ERROR if
the CERTIFICATE does not have a properly formatted time."
  (let ((asn1-time (x509-get0-not-before certificate)))
    (when (null-alien asn1-time)
      (error "X509_get0_notBefore returned NULL"))
    (decode-asn1-time asn1-time)))

(defun certificate-fingerprint (certificate &optional (algorithm :sha1))
  "Return the fingerprint of CERTIFICATE as a byte-vector. ALGORITHM is a string
designator for the digest algorithm to use (it defaults to SHA-1)."
  (tls:ensure-ssl)
  (let ((evp (evp-get-digest-by-name (string algorithm))))
    (when (null-alien evp)
      (error 'ssl-error-call
             :message (format nil "unknown digest algorithm ~A" algorithm)
             :queue (read-openssl-error-queue)))
    (let* ((size (funcall 'evp-md-get-size evp))
           (fingerprint (io:make-static-vector size)))
      (with-vector-sap (buf fingerprint)
        (unless (= 1 (x509-digest certificate evp buf nil))
          (error 'ssl-error-call
                 :message "failed to compute fingerprint of certificate"
                 :queue (read-openssl-error-queue))))
      fingerprint)))

