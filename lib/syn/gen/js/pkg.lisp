;;; js/pkg.lisp --- Javascript Code Generator

;; Lisp -> Javascript

;;; Commentary:

;; The current state-of-the-art CL->JS transpiler is Parenscript which hasn't
;; been officially patched in many years. This module is a port of Parenscript
;; using our SYN/GEN machinery which more closely resembles C-MERA.

;; In addition to JS.LISP file support, our other main goal is to support a
;; WITH-JS macro which is designed to work well with the WITH-HTML and
;; WITH-CSS macros. For embedding JS documents and snippets into a stream.

;; ref: https://parenscript.common-lisp.dev

;;; Code:
(defpackage :syn/gen/js
  (:nicknames :gen/js :js)
  (:use :cl :syn/gen))

(in-package :syn/gen/js)

(define-gen-backend :js :syn/gen/js)
