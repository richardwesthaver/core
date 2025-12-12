;;; pkg.lisp --- low-level bindings to OpenSSL (libcrypto)

;; OpenSSL

;;; Commentary:

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
   :pem-write
   :pem-read
   :ssl-free
   :openssl-add-all-algorithms
   :openssl-config
   :invalid-asn1-string
   :server-certificate-missing
   :asn1-error
   :err-r
   :openssl-condition
   :openssl-error
   :openssl-sap-error
   :openssl-error-initialize
   :openssl-error-want-something
   :openssl-error-none
   :openssl-error-zero-return
   :openssl-error-want-read
   :openssl-error-want-write
   :openssl-error-want-connect
   :openssl-error-want-x509-lookup
   :openssl-error-syscall
   :openssl-error-verify
   :openssl-error-call
   :read-openssl-error-queue
   :format-error-queue
   :ssl-get-error
   :ssl-connect
   :ssl-accept
   :ssl-write
   :ssl-read
   :ssl-shutdown
   :ssl-ctx-free
   :ssl-set-alpn-protos
   :ssl-get0-alpn-selected
   :ssl-ctx-set-default-verify-paths
   :ssl-ctx-set-default-verify-dir
   :ssl-ctx-set-default-verify-file
   :rsa-generate-key
   :rsa-free
   :bio-ctrl
   :bio-new-socket
   :bio-new
   :bio-get-new-index
   :bio-meth-new
   :bio-meth-set-puts
   :bio-meth-set-write
   :bio-meth-set-read
   :bio-meth-set-gets
   :bio-meth-set-create
   :bio-meth-set-destroy
   :bio-meth-set-ctrl
   :bio-set-init
   :bio-set-flags
   :bio-clear-flags
   :bio-test-flags
   :ssl-new
   :ssl-set-bio
   :ssl-get-fd
   :ssl-set-fd
   :ssl-set-connect-state
   :ssl-set-accept-state
   :tls-method))

(in-package :openssl)

(define-alien-loader ssl "/usr/lib/")
(define-alien-loader crypto "/usr/lib/")

(defconstant +BIO-FLAGS-IN-EOF+ #x800)

(defun tls-method ()
  (sb-alien::extern-alien "TLS_method" (* ssl-method)))
