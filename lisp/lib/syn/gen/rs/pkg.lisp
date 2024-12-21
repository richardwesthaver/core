;;; rs/pkg.lisp --- Rust Code Generator

;; Lisp -> Rust

;;; Commentary:

;; This code is bootstrapped from a tiny DSL I made for working with cbindgen
;; a few years back. Here's the header comment in that file:

#|
;; So basically, this was born out of personal frustration with how
;; cbindgen and Rust macros work (they don't). Rust macros in general
;; are something of a pain in my opinion, so I thought why not just
;; generate Rust code from Lisp instead?
|#

;; ref: 
;; ref: https://rust-lang.github.io/rfcs/3424-cargo-script.html
;; ref: https://github.com/rust-analyzer/ungrammar/blob/master/rust.ungram

;;; Code:
(defpackage :syn/gen/rs
  (:nicknames :gen/rs)
  (:use :cl :syn/gen :cli/tools/rust :ast :id :std/pipe :std/meta :syn/gen/c)
  (:import-from :std :in-readtable :eval-always)
  (:import-from :doc :file-header)
  (:shadow :cl-reader :else :body)
  (:export
   #:*rs-backend*
   #:*rs-symbols*
   #:*rs-syntax*
   #:*rs-reserved*
   #:*rs-exports*
   #:*rs-swap*
   #:rs-syntax
   #:gen-rs
   #:read-gen-rs-file
   #:read-gen-rs-string
   #:rs-reader-switch
   #:rs-reader))

(defpackage :syn/gen/rs/swap)

(in-package :syn/gen/rs)

(defvar *rs-backend*
  (append *cl-symbols* '()))

(export *rs-backend*)

(defparameter *rs-symbols* 
  '(<= >= < > + - * / = 
    for while return break continue if type let loop))

(defparameter *rs-syntax*
  '(|| && == != % << >> ^ | & += /= *= %= >>= <<= -= |= &= ^=
    pub crate enum struct mod fn extern else super as
    const impl in match move mut ref self static safe unsafe use where
    ;; weak keywords
    macro-rules union
    ;; 2018+
    async await dyn))

(defparameter *rs-reserved*
  '(abstract become do final macro override priv typeof unsized virtual yield
    ;; 2018+
    try))

(defparameter *rs-exports* (append *rs-symbols* *rs-syntax* *cl-symbols*))

(defparameter *rs-swap* (append *rs-symbols* *rs-syntax*))

(pkg:defpackage* :syn/gen/rs/sym
    (:shadow-symbols *rs-symbols* :export-symbols *rs-exports*)
  (:nicknames :rs)
  (:use :syn/gen/rs))
