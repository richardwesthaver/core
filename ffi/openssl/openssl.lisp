;;; aws-lc.lisp --- AWS-LC Alien Routines

;;

;;; Code:
(in-package :openssl)

(defar ("ASN1_STRING_data" asn1-string-data) (* unsigned-char)
  (str (* t)))

(defar ("ASN1_STRING_length" asn1-string-length) int
  (str (* t)))

(defar ("ASN1_STRING_type" asn1-string-type) int
  (str (* t)))

(defar ("ASN1_STRING_free" asn1-string-free) void
  (str (* t)))

(defar ("ASN1_TIME_check" asn1-time-check) int
  (time (* t)))

(defar ("ASN1_UTCTIME_check" asn1-utctime-check) int
  (time (* t)))

(defar ("d2i_X509" d2i-x509) (* x509)
  (out (* (* x509)))
  (inp (* (* unsigned-char)))
  (len long))

(defar ("ERR_get_error" err-get-error) unsigned-int)

(defar ("ERR_error_string" err-error-string) c-string
  (e unsigned-int)
  (buf (* char)))

(defconstant +err-error-string-buf-len+ 120)

(defar ("PEM_read" pem-read) int
  (fp (* int)) 
  (name (* c-string)) 
  (header (* c-string)) 
  (data (* (* unsigned-char))) 
  (len (* long)))

(defar ("PEM_write" pem-write) int
  (fp (* int))
  (name c-string)
  (header c-string)
  (data (* unsigned-char))
  (len long))

(defar ("PEM_def_callback" pem-def-callback) int
  (buf (* char)) 
  (size int) 
  (rwflag int) 
  (userdata (* t)))
