;;; ublk.lisp --- low-level bindings to libulksrv

;;; Commentary:

;;; Code:
(defpackage :ublk
  (:use :cl :std :sb-alien :uring)
  (:export :load-ublksrv))

(in-package :ublk)

(define-alien-loader ublksrv)
