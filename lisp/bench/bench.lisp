;;; bench.lisp --- Core Benchmarks

;; 

;;; Code:
(in-package :core/bench)

;; (setf (sb-ext:bytes-consed-between-gcs) 25000000)
(defun run-benchmark (bench)
  (ecase bench
    (:db-stress (in-package :bench/db/stress))
    (:tpc-h (in-package :bench/tpc-h))
    (:db-log (in-package :bench/db/log))
    (:lan-party 
     (start (make-instance 'bench/net/lan-party::lan-node)))))
