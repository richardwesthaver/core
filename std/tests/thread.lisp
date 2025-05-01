;;; thread.lisp --- Thread Tests

;; 

;;; Code:
(in-package :std/tests)
(in-suite :std)

(deftest with-threads ()
  "Test with-threads macro."
  (with-threads (4 :args (&optional (a 0) (b 1) (c 2)))
    (is (= 3 (+ a b c)))))

(deftest threads ()
  "Test standard thread functionality."
  (is (eq *current-thread*
          (find (thread-name *current-thread*) (list-all-threads)
                :key #'thread-name :test #'equal)))
  (is (find-thread-by-id (car (thread-id-list))))
  (is (not (zerop (thread-count))))
  (let ((threads
          (make-threads 4 (lambda () (is (= 42 (1+ 41)))) :name "threads")))
    (loop for th in threads
          do (sb-thread:join-thread th))
    (loop for th in threads
          collect (is (not (sb-thread:thread-alive-p th)))))
  (let ((m (make-mutex :name "mutex-test")))
    (is
     (and (not
           (with-mutex (m)
             (join-thread
              (make-thread (lambda ()
                             (with-mutex (m :timeout 0.1)
                               t))))))
          (join-thread
           (make-thread (lambda ()
                          (with-mutex (m :timeout 0.1)
                            t)))))))
  (let* ((sym (gensym))
         (s (make-semaphore :name "semaphore-test"))
         (th (make-thread (lambda () (wait-on-semaphore s)))))
    (is (equal (multiple-value-list (join-thread th :timeout .001 :default sym))
               (list sym :timeout)))
    (signal-semaphore s)
    (is (join-thread th)))
  (signals join-thread-error (join-thread *current-thread*))
  (is
   (let ((m (make-mutex :name "rlock-test")))
     (is (not (with-mutex (m) (join-thread (make-thread (lambda () (with-recursive-lock (m :wait-p nil) t)))))))
     (join-thread (make-thread (lambda () (with-recursive-lock (m :wait-p nil) t))))))
  (let ((queue (make-waitqueue :name "queue-test"))
        (lock (make-mutex :name "lock-test"))
        (n 0)
        th)
    (labels ((in-new-thread ()
               (with-mutex (lock)
                 (assert (eql (mutex-owner lock) *current-thread*))
                 (condition-wait queue lock)
                 (assert (eql (mutex-owner lock) *current-thread*))
                 (is (= n 1))
                 (decf n))))
      (setf th (make-thread #'in-new-thread))
      (sleep 1)
      (is (null (mutex-owner lock)))
      (with-mutex (lock)
        (incf n)
        (condition-notify queue))
      (is (= 0 (join-thread th))))))

(deftest timers ()
  "Test various timer functionality."
  (sb-int:with-progressive-timeout (ttl :seconds 1)
    (sleep 0.1)
    (is (/= (ttl) 1))))

(deftest thread-pool ()
  "Test THREAD-POOLs."
  (let ((tp (make-thread-pool 8)))
    (istype 'thread-pool tp)))
