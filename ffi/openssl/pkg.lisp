;;; pkg.lisp --- low-level bindings to AWS_LC (libcrypto)

;; OpenSSL

;;; Commentary:

;; ref: https://ffi.rustls.dev/

;; ref: https://docs.rs/rustls/latest/rustls/

;; ref: https://certifi.io/

;; ref: https://www.ccadb.org/resources

;; ccadb_url: https://ccadb.my.salesforce-sites.com/mozilla/IncludedCACertificateReportPEMCSV

;; mozilla exclusion? "9A296A5182D1D451A2E37F439B74DAAFA267523329F90F9A0D2007C334E23C9A"

;;; Code:
(defpackage :openssl
  (:use :cl :sb-alien :std/alien)
  (:export 
   :load-crypto :load-ssl
   :v-asn1 :v-asn1*
   :asn1-string
   :asn1-utctime-check
   :asn1-time-check
   :asn1-string-free
   :asn1-string-type
   :asn1-string-length
   :asn1-string-data
   :d2i-x509
   :aws-lc-c-error
   :aws-lc-condition
   :read-aws-lc-error-queue
   :aws-lc-error-call
   :pem-write
   :pem-read
   :openssl-add-all-algorithms
   :openssl-config))

(in-package :openssl)

(define-alien-loader ssl "/usr/local/lib/")
(define-alien-loader crypto "/usr/local/lib/")

