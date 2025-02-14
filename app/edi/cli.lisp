;;; cli.lisp --- EDI CLI

;; 

;;; Code:
(defpackage :edi/cli
  (:use :cl :std :edi :cli)
  (:export :edi-cli))

(in-package :edi/cli)
(defmain edi-cli ())
