;;; pkg.lisp --- low-level bindings to Linux syslog

;;; Commentary:

;; also exporting some additional syscalls here cuz why not.

;; gettid

;;; Code:
(defpackage :syslog
  (:use :cl :std :sb-alien)
  (:export 
   :closelog :openlog :setlogmask :syslog 
   :syslog-option :syslog-option*
   :syslog-facility :syslog-facility*
   :syslog-priority :syslog-priority*))
