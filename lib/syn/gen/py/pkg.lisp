;;; py/pkg.lisp --- Python Codegen

;; 

;;; Commentary:

;; Python 3+ only

;;; Code:
(defpackage :syn/gen/py
  (:nicknames :gen/py)
  (:use :cl :syn/gen :syn/ts))

(pkg:defpackage* :syn/gen/py/sym
  ()
  (:nicknames :py)
  (:use :cl)
  (:import-from :syn/gen :quoty :print-code :write-code :gen-package :code-print :cl-reader))

(in-package :syn/gen/py)

(defvar *py-symbols*)
