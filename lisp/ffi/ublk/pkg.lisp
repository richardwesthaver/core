;;; ublk.lisp --- low-level bindings to libulksrv

;;; Commentary:

;;; Code:
(defpackage :ublk
  (:use :cl :std :sb-alien)
  (:export ))

(in-package :ublk)

(define-alien-loader "ublksrv" t)
