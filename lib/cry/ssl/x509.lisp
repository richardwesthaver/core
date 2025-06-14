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
(defpackage :ssl/x509
  (:nicknames :x509)
  (:use :cl :std :dat/asn1))

(in-package :ssl/x509)

(defun decode-der-octet-vector (bytes))
