;;; tpc-h.lisp --- TPC-H Benchmark Suite

;; This package contains an implementation of the TPC-H benchmark.

;;; Code:
(defpackage :core/bench/tpc-h
  (:nicknames :bench/tpc-h)
  (:use :cl :std :rt/bench :log))

(in-package :core/bench/tpc-h)
