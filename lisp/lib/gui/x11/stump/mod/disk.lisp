;;; disk.lisp --- StumpWM Disk Mod

;; Uses IO/DISK internally

;;; Code:
(defpackage :gui/x11/stump/mod/disk
  (:nicknames :stump/disk)
  (:shadowing-import-from :stumpwm :group :message)
  (:use :cl :std :stumpwm :gui/x11/stump :disk))

(in-package :stump/disk)

(add-screen-mode-line-formatter #\D 'disk-modeline)

(defparameter *disk-formatters-alist*
  '((#\d  disk-get-device)
    (#\s  disk-get-size)
    (#\u  disk-get-used)
    (#\a  disk-available-space)
    (#\p  disk-use-percent)
    (#\m  disk-get-mount-point)
    (#\f  disk-get-filesystem-type)))
