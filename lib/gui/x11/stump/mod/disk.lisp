;;; disk.lisp --- StumpWM Disk Mod

;; Uses IO/DISK internally

;;; Code:
(defpackage :gui/x11/stump/mod/disk
  (:nicknames :stump/disk)
  (:use :cl :stumpwm :gui/x11/stump :disk))

(in-package :stump/disk)

(add-screen-mode-line-formatter #\D 'disk-modeline)

(defparameter *disk-formatters-alist*
  '((#\d  mountpoint-device)
    (#\s  mountpoint-size)
    (#\a  disk-available-space)
    (#\p  disk-use-percent)
    (#\m  mountpoint-directory)
    (#\f  mountpoint-fstype)))
