;;; bench.lisp --- Core Benchmarks

;; 

;;; Code:
(in-package :core/bench)

;; (setf (sb-ext:bytes-consed-between-gcs) 25000000)
(defun run-benchmark (bench)
  (ecase bench
    (:tpc-h (bench/tpc-h:tpc-h-benchmark))
    (:lan-party (start (make-instance 'bench/lan-party::lan-node)))))
     
