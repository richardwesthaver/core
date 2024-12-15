;;; rs/pkg.lisp --- Rust Code Generator

;; Lisp -> Rust

;;; Commentary:

;; initial set of tokens taken from rust-analyzer's ungrammar:
;; https://github.com/rust-analyzer/ungrammar/blob/master/rust.ungram

;; This code is bootstrapped from a tiny DSL I made for working with cbindgen
;; a few years back. Here's the header comment in that file:

#|
;; So basically, this was born out of personal frustration with how
;; cbindgen and Rust macros work (they don't). Rust macros in general
;; are something of a pain in my opinion, so I thought why not just
;; generate Rust code from Lisp instead?
|#

;; ref: https://rust-lang.github.io/rfcs/3424-cargo-script.html

;;; Code:
(defpackage :syn/gen/rs
  (:nicknames :gen/rs)
  (:use :cl :syn/gen :cli/tools/rust :ast :id :std/pipe :std/meta)
  (:import-from :std :in-readtable :eval-always))

(in-package :syn/gen/rs)

(defmethod load-generator ((self (eql :rs))) :rs)
(defmethod generator-package ((self (eql :rs))) :syn/gen/rs/sym)

(defvar *rs-backend*
  (append *cl-symbols* nil))

(export *rs-backend*)

(defparameter *rs-symbols* '())
(defparameter *rs-syntax* '())
(defparameter *rs-exports* (append *rs-symbols* *rs-syntax* *cl-symbols*))
(defparameter *rs-swap* (append *rs-symbols* *rs-syntax*))  

(pkg:defpackage* :syn/gen/rs/sym
    (:shadow-symbols *rs-symbols* :export-symbols *rs-exports*)
  (:use :syn/gen/rs))
