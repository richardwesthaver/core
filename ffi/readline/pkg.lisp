;;; pkg.lisp --- low-level bindings to librustls

;;; Commentary:

;; https://github.com/vindarel/cl-readline

;;; Code:
(defpackage :readline
  (:use :cl :sb-alien :std/alien)
  (:export :load-readline
           :rl :readline
           :recent-history-line-satisfies-p
           :*rl-history-base*
           :*rl-history-length*))

(in-package :readline)

(define-alien-loader readline "/usr/lib/")
