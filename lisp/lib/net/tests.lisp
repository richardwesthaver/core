(defpackage :net/tests
  (:use :rt :std :cl :net :sb-concurrency :sb-thread))

(in-package :net/tests)

(defsuite :net)
(in-suite :net)

(deftest sanity ())

(deftest dns ())

(deftest tcp ())

(deftest udp ())

(deftest tlv ())

(deftest osc ())

(deftest crew ()
    (let ((pool (make-worker-pool (make-instance 'crew-connection-info :host-name "localhost" :port 9999)
                                  (list (make-instance 'crew-connection-info :host-name "localhost" :port 10000))
                                  #'connect-worker)))
      (let* ((worker-count (if (null pool) 1 (worker-count pool)))
             (work '(cons 1 2))
             (expected-result (make-list worker-count :initial-element '(1 . 2)))
             (count 0)
             (count-lock (make-mutex :name "count")))
        (flet ((result-done (position element)
                 (with-mutex (count-lock)
                   (incf count)
                   (is (equal (nth position expected-result) element)))))
          (is (equal (eval-form-all-workers pool work :replay-required nil) expected-result))
          (is (equal (eval-form-all-workers pool work :result-done #'result-done :replay-required nil)
                     expected-result))
          (is (= count worker-count))))))

  ;; (defun test-eval-form-repeatedly (pool)
  ;;   (let ((worker-count (if (null pool) 1 (worker-count pool))))
  ;;     (is (equal (eval-form-repeatedly pool 0 '(constantly 42)) '()))
  ;;     (is (equal (eval-form-repeatedly pool 10 '(constantly (cons 1 2)))
  ;;                (make-list 10 :initial-element (cons 1 2))))
  ;;     (is (equal (eval-form-repeatedly pool 20 '(constantly (cons 3 4))
  ;;                                      :worker-count (floor (/ worker-count 2)))
  ;;                (make-list 20 :initial-element (cons 3 4))))
  ;;     (is (equal (eval-form-repeatedly pool 30 '(constantly (cons 5 6)) :worker-count 0)
  ;;                (make-list 30 :initial-element (cons 5 6))))))

  ;; (defun test-parallel-mapcar (pool)
  ;;   (let ((input '(100 200 300))
  ;;         (expected-result '((100 . 1) (200 . 1) (300 . 1)))
  ;;         (count 0))
  ;;     (flet ((result-done (position element)
  ;;              (incf count)
  ;;              (is (equal (nth position expected-result) element))))
  ;;       (is (equal (parallel-mapcar pool (lambda (x) `(cons ,x 1)) input) expected-result))
  ;;       (is (equal (parallel-mapcar pool (lambda (x) `(cons ,x 1)) input #'result-done)
  ;;                  expected-result))
  ;;       (is (= count (length expected-result))))))

  ;; (defun test-parallel-reduce (pool)
  ;;   (is (equal (parallel-reduce pool
  ;;                               (lambda (x) `(list ,x 1))
  ;;                               '(100 200 300)
  ;;                               '(a b c)
  ;;                               #'append)
  ;;              '(a b c 100 1 200 1 300 1))))

  ;; (defun test-eval-repeatedly-async-state (pool)
  ;;   (let ((expected-state 10)
  ;;         (update-count 0)
  ;;         (work-form '(lambda (state)
  ;;                      ;; Return results slowly so we don't create huge result lists.
  ;;                      (sleep 0.1)
  ;;                      (* state state))))
  ;;     (flet ((update-state (state results)
  ;;              (is (= state expected-state))
  ;;              (is (not (null results)))
  ;;              (dolist (result results)
  ;;                (is (or (= result (expt state 2))
  ;;                        (= result (expt (1- state) 2))
  ;;                        (= result (expt (- state 2) 2)))))
  ;;              ;; Allow time for several results to accumulate.
  ;;              (sleep 0.5)
  ;;              (values (incf expected-state) (> (incf update-count) 3) t)))
  ;;       (eval-repeatedly-async-state pool work-form 10 #'update-state :worker-count 0)
  ;;       (setf expected-state 10
  ;;             update-count 0)
  ;;       (eval-repeatedly-async-state pool work-form 10 #'update-state))))

  (deftest crew ())
