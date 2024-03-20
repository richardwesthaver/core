;;; ublk.lisp --- low-level bindings to CUBLK

;;; Commentary:

;;; Code:
(defpackage :ublk/pkg
  (:nicknames :ublk)
  (:use :cl :std :sb-alien)
  (:export ))

(in-package :ublk)

(define-alien-loader "ublksrv" t)
