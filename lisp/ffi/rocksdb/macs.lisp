;;; rocksdb/macs.lisp --- macros used to generate the alien API

;;; Code:
(defpackage :rocksdb/macs
  (:use :cl :std)
  (:export
   :def-rocksdb-opt))

(in-package :rocksdb/macs)

(defmacro def-opt-getter (opt &rest args)
  `(define-alien-routine ,opt (* ,opt) ,args))

(defmacro def-opt-setter (opt &rest args)
  `(define-alien-routine ,opt (* ,opt) ,args))

(defmacro def-rocksdb-opt (opt))
