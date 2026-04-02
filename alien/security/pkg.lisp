;;; pkg.lisp --- low-level bindings to Linux security modules

;;; Commentary:

;;; Code:
(defpackage :security
  (:use :cl :std :sb-alien)
  (:export :linux-pam :linux-pam-minor))
