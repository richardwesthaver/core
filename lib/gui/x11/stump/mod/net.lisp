;;; net.lisp --- StumpWM NET Module

;; 

;;; Code:
(defpackage :gui/x11/stump/mod/net
  (:nicknames :stump/net)
  (:use :cl :stumpwm :gui/x11/stump :cli/tools/net :net)
  (:export :*net-modeline-fmt* :*net-stat-devices*))

(in-package :stump/net)

(add-screen-mode-line-formatter #\l 'net-mode-line)
