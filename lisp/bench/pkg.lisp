;;; bench.lisp --- Core Benchmarks

;; 

;;; Code:
(in-package :std-user)

(defpkg :core/bench
  (:use :std-lisp :rt :log :rt/bench :rt/cover)
  (:export :*bench-directory* :bench-path))

(in-package :core/bench)

;; (setf (sb-ext:bytes-consed-between-gcs) 25000000)
