;;; disk.lisp --- StumpWM Disk Mod

;; Uses IO/DISK internally

;;; Code:
(defpackage :x11/stump/mod/disk
  (:nicknames :stump/disk)
  (:shadowing-import-from :stumpwm :group :message)
  (:use :cl :std :stumpwm :x11/stump :disk))

(in-package :stump/disk)
