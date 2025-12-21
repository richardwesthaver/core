;;; cu/pkg.lisp --- CUDA Code Generator

;; Generate CUDA C code (.cu)

;;; Commentary:

;; This package derives mostly from C-MERA with inspiration from CL-CUDA. The
;; AST must be compiled to a .cu file so that NVCC may compile it into a CUDA
;; kernel module (.ptx file).

;; To use the compiled kernel modules from Common Lisp see the MATH/CUDA
;; package.

;;; Code:
(defpackage :syn/gen/cu
  (:nicknames :gen/cu)
  (:use :syn/gen :syn/gen/c :ast :id)
  (:shadow :cl-reader)
  (:export
   #:*cu-backend*
   #:*cu-symbols*
   #:*cu-exports*
   #:*cu-swap*))

(in-package :syn/gen/cu)

(defmethod gen-package ((self (eql :cu))) (find-package :syn/gen/cu/sym))

(defparameter *cu-backend*
  (append syn/gen/cpp:*cpp-backend*
          '(size cuda-alignment shared threads
            blocks cuda-funcall)))

(defparameter *cu-symbols*
  syn/gen/cpp:*cpp-symbols*)

(defparameter *cu-syntax*
  (append syn/gen/cpp:*cpp-syntax*
          '(launch)))

(defparameter *cu-exports*
  (append *cu-symbols*
          *cu-syntax*
          *cl-symbols*))

(defparameter *cu-swap*
  (append *cu-symbols* *cu-syntax*))

;; (export *cu-backend*)
(pkg:defpackage* :syn/gen/cu/swap
  (:shadow-symbols *cu-swap*))

(pkg:defpackage* :syn/gen/cu/sym
  (:shadow-symbols () :export-symbols *cu-exports*)
  (:nicknames :cu)
  (:shadow :struct)
  (:use :syn/gen/cpp/sym)
  (:import-from :syn/gen/cpp :decompose-declaration))

(define-gen-backend :cu :syn/gen/cu :sym :syn/gen/cu/sym :swap :syn/gen/cu/swap)
