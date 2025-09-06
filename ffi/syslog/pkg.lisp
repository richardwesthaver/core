;;; pkg.lisp --- low-level bindings to Linux syslog

;;; Commentary:

;;; Code:
(defpackage :syslog
  (:use :cl :std :sb-alien)
  (:export :closelog :openlog :setlogmask :syslog))

(in-package :syslog)
