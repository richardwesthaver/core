;;; perf.lisp --- Core Performance Analysis Script

;; Record Linux perf data and run benchmarks.

;;; Code:
(in-package :user)
(require 'sb-perf)
(sb-perf:write-jitdump)
;; initialize the default thread-pool
(start
 (or (find-thread-pool :default)
     (make-thread-pool (num-cpus) :name :default :alive t)))

;; start recording, run until exit
(cli/tools:perf-record 
 "-k" "mono" "-g" "-p" (format nil "~A" (sb-posix:getpid)) "perf.data")
;; Code to record
;; ...
;; Stop recording (kill process)

;; (cli/tools:perf-inject-jit)
;; perf report -i perf.jit.data
