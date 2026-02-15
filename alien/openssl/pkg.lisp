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
   :ssl-set-default-passwd-cb
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
   :tls-method
   :bio-free
   :+ssl-error-none+
   :+ssl-error-ssl+
   :+ssl-error-want-read+
   :+ssl-error-want-write+
   :+ssl-error-want-x509-lookup+
   :+ssl-error-syscall+
   :+ssl-error-zero-return+
   :+ssl-error-want-connect+
   :x509-free
   :x509-name-oneline
   :x509-name-get-index-by-nid
   :x509-name-get-entry
   :x509-name-entry-get-data
   :x509-get-issuer-name
   :x509-get-subject-name
   :x509-get0-not-before
   :x509-get0-not-after
   :x509-get-ext-d2i
   :x509-store-ctx-get-error
   :ssl-set-cipher-list
   :+NID-subject-alt-name+
   :+NID-commonName+
   :openssl-sk-num
   :openssl-sk-value
   :general-names-free
   :evp-get-digest-by-name
   :evp-md-get-size
   :pem-read-bio-x509
   :pem-write-bio-x509
   :x509-digest
   :+SSL-CTRL-OPTIONS+
   :+SSL-CTRL-SET-SESS-CACHE-MODE+
   :+SSL-CTRL-MODE+))

(in-package :openssl)

(define-alien-loader ssl "/usr/lib/")
(define-alien-loader crypto "/usr/lib/")

(defconstant +BIO-FLAGS-IN-EOF+ #x800)
(defconstant +SSL-CTRL-SET-TLSEXT-HOSTNAME+ 55)
(defconstant +SSL-CTRL-OPTIONS+ 32)
(defconstant +SSL-CTRL-SET-SESS-CACHE-MODE+ 44)
(defconstant +SSL-CTRL-MODE+ 33)

(defconstant +NID-subject-alt-name+ 85)
(defconstant +NID-commonName+   13)

(defconstant +GEN-OTHERNAME+  0)
(defconstant +GEN-EMAIL+  1)
(defconstant +GEN-DNS+    2)
(defconstant +GEN-X400+ 3)
(defconstant +GEN-DIRNAME+  4)
(defconstant +GEN-EDIPARTY+ 5)
(defconstant +GEN-URI+    6)
(defconstant +GEN-IPADD+  7)
(defconstant +GEN-RID+    8)

(defconstant +v-asn1-octet-string+ 4)
(defconstant +v-asn1-utf8string+ 12)
(defconstant +v-asn1-printablestring+ 19)
(defconstant +v-asn1-teletexstring+ 20)
(defconstant +v-asn1-iastring+ 22)
(defconstant +v-asn1-universalstring+ 28)
(defconstant +v-asn1-bmpstring+ 30)
