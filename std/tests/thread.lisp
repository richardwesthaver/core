;;; thread.lisp --- Thread Tests

;; 

;;; Code:
(in-package :std/tests)
(in-suite :std)

(deftest with-threads ()
  "Test with-threads macro."
  (with-threads (i 4 :args (&optional (a 0) (b 1) (c 2)))
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
    (sleep 0.001)
    (is (/= (ttl) 1))))

(deftest temp-pool ()
  "Test THREAD-POOLs."
  (with-temp-pool (100 :alive t)
    (istype '(array worker) (workers*))
    (istype 'biased-scheduler (scheduler*))
    (is= 4 (length (workers*)))
    (istype 'thread-pool *thread-pool*)
    (is= 100 (reduce '+ (broadcast-work (lambda () 1))))
    (let ((ch (make-channel)))
      (submit-work ch (lambda () :foo))
      (iseql :foo (receive-result ch)))
    (with-submit-indexed 10 (make-array 10)
      (dotimes (i 10)
        (submit-indexed i (lambda () (is= 4 (+ 2 2)))))
      (receive-indexed))
    (submit-with-cancel
      (submit-cancelable (lambda () (is t)))
      (submit-cancelable (lambda () (isnt nil)))
      (receive-cancelables))))

(deftest basic-threading-test ()
  (let ((num-threads 10)
        (num-objects 1000)
        (num-iterations 5)
        (from-workers (make-queue))
        (to-workers (make-queue)))
    (repeat num-threads
      (with-thread ()
        (loop (let ((object (pop-queue to-workers)))
                (if object
                    (push-queue object from-workers)
                    (return))))))
    (repeat num-iterations
      (repeat num-objects
        (push-queue 99 to-workers))
      (repeat num-objects
        (pop-queue from-workers)))
    (repeat num-threads
      (push-queue nil to-workers))
    (sleep 0.5)
    (is (= 0 (queue-count from-workers)))
    (is (= 0 (queue-count to-workers)))))

(defparameter *memo* t)

(deftest thread-bindings-test ()
  (setq *memo* :main)
  (with-thread ()
    (setf *memo* :child))
  (sleep 0.2)
  (is (eql :child *memo*))
  (setf *memo* :main)
  (with-thread (:bindings `((*memo* ,*memo*)))
    (sleep 0.2)
    (setf *memo* :child))
  (iseql :main *memo*))

(deftest terminate-thread-cleanup-test ()
  (let* ((cleanedp nil)
         (thread (with-thread ()
                   (unwind-protect (sleep 999999)
                     (setf cleanedp t)))))
    (sleep 0.2)
    (terminate-thread thread)
    (sleep 0.2)
    (is (eq t cleanedp))))
