;;; tests/task.lisp --- Task Tests

;; 

;;; Code:
(in-package :std/tests)
(in-suite :std)

(deftest tasks ()
  "Test task-pools, oracles, and workers."
  (with-threads (4 :args (&optional (a 0) (b 1) (c 2)))
    (is (= 3 (+ a b c))))
  ;; *ORACLE-THREADS* contains the *CURRENT-THREAD*.
  (std/task:with-task-pool (tp :count 10 :spawn 4)
    (is (= 4 (length (task-pool-workers tp))))
    (std/task::task-pool-lock tp)
    (is (= 4 (std/task::mailbox-count (task-pool-results tp))))
    (describe tp)
    (dotimes (i 4)
      (is (eql t (std/task::receive-message (task-pool-results tp)))))
    (is (null (std/task::receive-message-no-hang (task-pool-results tp))))))
