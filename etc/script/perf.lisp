#!/bin/core --script
#|Record Linux perf data|#
(in-package :user)
(require 'sb-perf)
(sb-perf:write-jitdump)
(when (string-equal (car (cli/clap::args)) "i")
  (cli/tools:perf-inject-jit)
  (sb-perf:write-perfmap)
  (quit))
;; initialize the default thread-pool
(start
 (or (find-thread-pool :default)
     (make-thread-pool (num-cpus) :name :default :alive t)))
(cli/tools:perf-record "-k" "mono" "-g" "-p" (format nil "~A" (sb-posix:getpid)))
(test-system :std)
(exit-thread-pools)
(quit)
;; perf report -i perf.jit.data
