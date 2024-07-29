;;; tests/task.lisp --- Task Tests

;; 

;;; Code:
(in-package :std/tests)
(in-suite :std)

(deftest tasks ()
  "Test task-pools, oracles, and workers."
  (let ((pool (designate-oracle (make-task-pool) (make-oracle *current-thread*))))
    ;; pool is bound to a task pool, *ORACLE-THREADS* contains the *CURRENT-THREAD*.
    (spawn-workers pool 16)
    ;; (with-threads (16 :args (&optional (a 0) (b 1) (c 2)))
    ;;   (sb-thread:allocator-histogram)
    ;;   (sb-concurrency:wait-on-gate (std/thread::task-pool-online pool))
    ;;   (print (+ a b c)))
    (is (= 16 (length (task-pool-workers pool))))
    (is (sb-thread:semaphore-count (std/task::task-pool-online pool)))))
