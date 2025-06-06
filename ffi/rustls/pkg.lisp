;;; pkg.lisp --- low-level bindings to librustls

;;; Commentary:

;; ref: https://docs.rs/rustls/latest/rustls/

;; ref: https://certifi.io/

;; ref: https://www.ccadb.org/resources

;; ccadb_url: https://ccadb.my.salesforce-sites.com/mozilla/IncludedCACertificateReportPEMCSV

;; mozilla exclusion? "9A296A5182D1D451A2E37F439B74DAAFA267523329F90F9A0D2007C334E23C9A"

;;; Code:
(defpackage :rustls
  (:use :cl :sb-alien :std/alien)
  (:export 
   :load-rustls
   :rustls-condition
   :rustls-c-error
   :rustls-result :rustls-result*
   :rustls-tls-version :rustls-tls-version*
   :rustls-handshake-kind :rustls-handshake-kind*
   :rustls-server-connection-new
   :rustls-server-connection-get-server-name
   :rustls-server-config-builder-new
   :rustls-server-config-builder-free
   :rustls-server-config-builder-build
   :rustls-server-config-free
   :rustls-client-connection-new
   :rustls-client-config-builder-new
   :rustls-client-config-builder-build
   :rustls-client-config-builder-free
   :rustls-client-config-free
   :rustls-version
   :rustls-acceptor-new
   :rustls-acceptor-free
   :rustls-acceptor-read-tls
   :rustls-acceptor-accept
   :rustls-accepted-server-name
   :rustls-accepted-signature-scheme
   :rustls-accepted-cipher-scheme
   :rustls-accepted-free
   :rustls-accepted-into-connection
   :rustls-certificate-get-der
   :rustls-supported-ciphersuite-get-suite
   :rustls-supported-ciphersuite-get-name
   :rustls-all-ciphersuites-len
   :rustls-all-ciphersuites-get-entry
   :rustls-default-ciphersuites-len
   :rustls-default-ciphersuites-get-entry
   :rustls-certified-key-build
   :rustls-certified-key-get-certificate
   :rustls-certified-key-free
   :rustls-root-cert-store-builder-new
   :rustls-root-cert-store-builder-add-pem
   :rustls-root-cert-store-builder-load-roots-from-file
   :rustls-root-cert-store-builder-build
   :rustls-root-cert-store-builder-free
   :rustls-root-cert-store-free
   :rustls-client-cert-verifier-free))

(in-package :rustls)

(define-alien-loader rustls "/usr/lib/")
