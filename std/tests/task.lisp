;;; tests/task.lisp --- Task Tests

;; 

;;; Code:
(in-package :std/tests)
(in-suite :std)

(deftest task-pool ()
  "Task Pool tests."
  (with-task-pool (tp :workers 4 :tasks 10)
    (is (= 4 (length (task-pool-workers tp))))
    (is (> (std/task::mailbox-count (task-pool-results tp)) 0))
    (dotimes (i 4)
      (is (null (std/task::receive-message (task-pool-results tp)))))
    (is (null (std/task::receive-message-no-hang (task-pool-results tp))))
    (kill-workers tp)
    (is (zerop (worker-count tp))))
  (with-task-pool (tp :workers 4 :tasks 4)
    (is (zerop (sb-concurrency:mailbox-count (results tp))))
    (start-task-workers tp)
    (loop for w across (workers tp)
          do (join-worker w))
    (is (= 4 (sb-concurrency:mailbox-count (results tp))))))
