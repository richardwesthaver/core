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
    (is (task-p t1))
    (is (task-p t2))
    (is (task-p t3))))

(deftest simple-task ()
  "Test simple tasks in sync/async contexts.")
    
;; TODO 2025-10-23: 
(deftest job ()
  "Basic JOB functionality."
  (let ((j1 (apply 'make-job (collecting (do ((i 0 (incf i))) ((= i 10)) (collect (make-task (constantly t) t)))))))
    (is (job-p j1))
    (is (task-p (aref (tasks j1) 0)))
    (is (vectorp (tasks j1)))
    (is= (length (tasks j1)) 10)
    #+nil
    (with-temp-pool (4 :worker-class 'task-worker :alive t)
      (run-job (aref (workers*) 0) j1))))
    
