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
  (:export :load-rustls))

(in-package :rustls)

(define-alien-loader rustls "/usr/lib/")
