;;; pkg.lisp --- low-level bindings to Linux systems

;;; Commentary:

;; syscalls, syslog, sockets, errors, signums

;; gettid

;;; Code:
(defpackage :sys
  (:use :cl :std :sb-alien)
  (:export :sigaction
   :err :err* :sig :sig*
   :epoll-data :epoll-event :epoll-create :epoll-create1
   :epoll-ctl :epoll-wait
   :closelog :openlog :setlogmask :syslog 
   :syslog-option :syslog-option*
   :syslog-facility :syslog-facility*
   :syslog-priority :syslog-priority*
   :rlimit :if-nameindex
   :if-nametoindex :if-indextoname
   :if-freenameindex :iovec
   :iov-base :iov-len
   :msghdr :kernel-timespec
   :at-fdcwd :block-uring-cmd-discard
   :open-how :msghdr :cmsghdr :linger
   :io-vector :io-vec))
