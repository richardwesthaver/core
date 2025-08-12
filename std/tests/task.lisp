;;; tests/task.lisp --- Task Tests

;; 

;;; Code:
(in-package :std/tests)
(in-suite :std)

(deftest task ()
  "Basic TASK functionality."
  (let ((t1 (make-instance 'task))
        (t2 (make-instance 'scheduled-task))
        (t3 (make-instance 'async-task)))
    (is (taskp t1))
    (is (taskp t2))
    (is (taskp t3))))

(deftest simple-task ()
  "Test simple tasks in sync/async contexts.")
    
(deftest job ()
  "Basic JOB functionality."
  (let ((j1 (make-instance 'job)))
    (is (jobp j1))))
