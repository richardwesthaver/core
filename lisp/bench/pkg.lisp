;;; bench.lisp --- Core Benchmarks

;; 

;;; Code:
(in-package :std-user)

(defpkg :core/bench
  (:nicknames :bench)
  (:use :std-lisp :rt :log :rt/bench))

(in-package :core/bench)

(defbench simple () "")
