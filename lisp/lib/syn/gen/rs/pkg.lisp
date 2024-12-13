;;; rs/pkg.lisp --- Rust Code Generator

;; Lisp -> Rust

;;; Commentary:

;; There are no existing ? -> Rust generators that I know of - not exactly an
;; intended use-case, as compile-times are quite length and the syntax is a
;; more complex version of C++ or ML.

;; ref: https://rust-lang.github.io/rfcs/3424-cargo-script.html

;; This code is bootstrapped from a tiny DSL I made for working with cbindgen
;; a few years back. Here's the header comment in that file:
#|
;; So basically, this was born out of personal frustration with how
;; cbindgen and Rust macros work (they don't). Rust macros in general
;; are something of a pain in my opinion, so I thought why not just
;; generate Rust code from Lisp instead?
|#

;;; Code:
(defpackage :syn/gen/rs
  (:nicknames :genrs :rs)
  (:use :cl :syn/gen :cli/tools/rust :obj/ast :std/pipe :std/meta)
  (:import-from :std :in-readtable :eval-always))

(in-package :syn/gen/rs)
(in-readtable :std)

(defvar *rs-macros* nil)
(defvar *default-cargo-target-directory* (merge-pathnames "target/" *default-pathname-defaults*))

(defmacro rs-defmacro (name args &body body)
  "Define a macro which can be used within the body of a 'with-rs' form."
  `(prog1
       (defmacro ,name ,@(mapcar #`(,a1) args) ,@body)
     (push ',name *rs-macros*)))

(defun rs-mod-form (crate &optional mods pub)
  "Generate a basic mod form (CRATE . [MODS] [PUB])"
  `(,crate ,mods ,pub))

(defmacro with-rs-env (imports &body body)
  "Generate an environment for use within a Rust generator macro."
  `(let ((imports ,(mapcar #'rs-mod-form imports)))
     (format nil "~A~&~A" imports ',body)))

(defun rs-use (crate &optional mods pub)
  "Generate a single Rust use statement."
  (concatenate
   'string
   (if pub "pub " "")
   "use " crate "::{"
   (cond
     ((consp mods)
      (reduce
       (lambda (x y) (format nil "~A,~A" x y))
       mods))
     (t mods))
   "};"))

(defun rs-mod (mod &optional pub)
  "Generate a single Rust mod statement."
  (concatenate
   'string
   (if pub "pub " "")
   "mod " mod ";"))

(defun rs-imports (&rest imports)
  "Generate a string of Rust 'use' statements."
  (cond
    ((consp imports)
     (mapcar (lambda (x) (apply #'rs-use (apply #'rs-mod-form x))) imports))
    (t imports)))

(defmacro rs-extern-c-fn (name args &optional pub unsafe no-mangle &body body)
  "Generate a Rust extern 'C' fn."
  `(concatenate
    'string
    ,(when no-mangle (format nil "#[no_mangle]~&"))
    ,(when pub "pub ")
    ,(when unsafe "unsafe ")
    "extern \"C\" fn " ,name "("
    ,(cond
       ((consp args) (reduce (lambda (x y) (format nil "~A,~A" x y)) args))
       (t args))
    ")" "{" ,@body "}"))

;; (defun rs-macroexpand-1 (form &optional env))

;; (defun rs-macroexpand (env &rest body)
