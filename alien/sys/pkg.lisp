;;; pkg.lisp --- low-level bindings to Linux systems

;;; Commentary:

;; syscalls, syslog, sockets, errors, signums

;; gettid

;;; Code:
(defpackage :sys
  (:use :cl :std :sb-alien)
  (:export 
   :closelog :openlog :setlogmask :syslog 
   :syslog-option :syslog-option*
   :syslog-facility :syslog-facility*
   :syslog-priority :syslog-priority*))
