;;; cu/pkg.lisp --- CUDA Code Generator

;; Generate CUDA C code (.cu)

;;; Commentary:

;; This package derives mostly from C-MERA which inspiration from CL-CUDA. The
;; AST must be compiled to a .cu file so that NVCC may compile it into a CUDA
;; kernel module (.ptx file).

;; To use the compiled kernel modules from Common Lisp see the MATH/CUDA
;; package.

;;; Code:
(defpackage :syn/gen/cu
  (:nicknames :gen/cu)
  (:use :syn/gen :syn/gen/c :syn/gen/cpp :std/pipe :std/seq :std/meta :cli/tools/cc :cli/env :id :ast)
  (:export
   #:*cu-backend*))

(in-package :syn/gen/cu)

(defmethod load-gen ((self (eql :cu))) 
  (init-gen :cu)
  ;; (cu-reader)
  )

(defmethod unload-gen ((self (eql :cu))) 
  (init-gen nil) 
  (cl-reader))

(defmethod gen-package ((self (eql :cu))) (find-package :syn/gen/cu))

(defparameter *cu-backend*
  (append *cpp-backend*
          '(size cuda-alignment shared threads
            blocks cuda-funcall)))

(defparameter *cu-symbols*
  *cpp-symbols*)

(defparameter *cu-syntax*
  (append *cpp-syntax*
          '(launch)))

(defparameter *cu-exports*
  (append *cu-symbols*
          *cu-syntax*
          *cl-symbols*))

(defparameter *cu-swap*
  (append *cu-symbols* *cu-syntax*))

(pkg:defpackage* :syn/gen/cu/swap
  (:shadow-symbols *cu-swap*))

(pkg:defpackage* :syn/gen/cu/sym
  (:shadow-symbols nil :export-symbols *cu-exports*)
  (:shadow :struct)
  (:use :syn/gen/cpp/sym)
  (:import-from :syn/gen/cpp :decompose-declaration))

(define-gen-backend :cu :syn/gen/cu :sym :syn/gen/cu/sym :swap :syn/gen/cu/swap)
