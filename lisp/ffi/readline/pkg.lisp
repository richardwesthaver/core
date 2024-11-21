;;; pkg.lisp --- low-level bindings to librustls

;;; Commentary:

;; https://github.com/vindarel/cl-readline

;;; Code:
(defpackage :readline
  (:use :cl :sb-alien :std/alien)
  (:export :load-readline
           :rl
           :recent-history-line-satisfies-p))

(in-package :readline)

(define-alien-loader readline "/usr/lib/")
