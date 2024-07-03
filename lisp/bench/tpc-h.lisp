;;; tpc-h.lisp --- TPC-H Benchmark Suite

;; This package contains an implementation of the TPC-H benchmark.

;;; Commentary:

;; ref: https://www.tpc.org/tpc_documents_current_versions/pdf/tpc-h_v2.17.1.pdf

;;; Code:
(defpackage :core/bench/tpc-h
  (:nicknames :bench/tpc-h :tpc-h)
  (:use :cl :std :rt/bench :rt/cover :log :sql :parse/pratt))

(in-package :core/bench/tpc-h)
