;;; disk.lisp --- StumpWM Disk Mod

;; Uses IO/DISK internally

;;; Code:
(defpackage :gui/x11/stump/mod/disk
  (:nicknames :stump/disk)
  (:use :cl :stumpwm :gui/x11/stump :disk)
  (:export :*disk-modeline-fmt*
           :*disk-usage-paths*))

(in-package :stump/disk)

(add-screen-mode-line-formatter #\D 'disk-mode-line)

(defparameter *disk-usage* nil)

(defparameter *disk-formatters-alist*
  '((#\d  disk-get-device)
    (#\s  disk-get-size)
    (#\u disk-get-used)
    (#\a  disk-get-available)
    (#\p  disk-get-use-percent)
    (#\m  disk-get-mount-point)
    (#\f  disk-get-filesystem-type)))

(defparameter *disk-modeline-fmt* "%m: %u/%s"
  "The default value for displaying disk usage information on the modeline.

%% = A literal '%'
%d = Filesystem device
%s = Filesystem size
%a = Filesystem available space
%p = Filesystem used space in percent
%m = Filesystem mount point
%f = Filesystem type
")

(defvar *disk-usage-paths* '("/" "/home/" "/opt/store/" "/opt/stash/" "/opt/backup/")
  "The list of mount points to report the disk usage of.")

(defun disk-update-usage (paths)
  (setf *disk-usage* (mapcar 'disk:disk-info paths)))

(defun disk-usage-get-field (path field-number)
  (let ((usage-infos (find-if (lambda (item)
                                (string= (car (last item)) path))
                              *disk-usage*)))
    (nth field-number usage-infos)))

(defun size-human-readable (size-as-number)
  (std:human-readable-size size-as-number))

(defun disk-get-size-as-number (path)
  (disk:disk-total-space path))

(defun disk-get-size (path)
  (disk:disk-total-space path t))

(defun disk-get-used-as-number (path)
   (- (disk-get-size-as-number path)
      (disk-get-available-size-as-number path)))

(defun disk-get-used (path)
  (size-human-readable (disk-get-used-as-number path)))

(defun disk-get-available-size-as-number (path)
  (disk:disk-available-space path))

(defun disk-get-available (path)
  (std:human-readable-size (disk:disk-available-space path t)))

(defun disk-get-use-percent (path)
  (let ((value (truncate (* 100
                            (/ (disk-get-used-as-number path)
                               (disk-get-size-as-number path))))))

    (format nil "~a%" value)))

(defun disk-get-device (path)
  #+linux
  (handler-case
      (disk:mountpoint-device path path)
    (error () "ERR"))
  #-linux (disk-usage-get-field path 0))

(defun disk-get-mount-point (path)
  path)

(defun disk-get-filesystem-type (path)
  #+linux
  (handler-case
      (disk:mountpoint-fstype path)
    (error () "ERR"))
  #-linux "filesystem type supported only on GNU/Linux :-(")

(defun use-fallback-method-p ()
  (search "%d" *disk-modeline-fmt* :test #'string=))

(defun disk-mode-line (ml)
  (declare (ignore ml))
  #-linux
  (when (use-fallback-method-p)
    (disk-update-usage *disk-usage-paths*))
  (let ((fmts (loop for p in *disk-usage-paths* collect
                   (format-expand *disk-formatters-alist*
                                  *disk-modeline-fmt*
                                  p))))
    (format nil "~{~a ~}" fmts)))
