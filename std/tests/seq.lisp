;;; seq.lisp --- Sequence Tests

;; 

;;; Code:
(in-package :std/tests)
(in-suite :std)

(deftest vector-queue ()
  (let ((q (make-queue :capacity 1000 :initial-contents (make-array 10 :element-type 'fixnum :initial-element 42))))
    (istype 'vector-queue q)
    (istype 'simple-vector (data q))
    (dotimes (i 10)
      (is= 42 (try-pop-queue q)))
    (is (queue-empty-p* q))
    (let ((th (make-thread (lambda () (is= most-positive-fixnum (try-pop-queue q))))))
      (push-queue* most-positive-fixnum q)
      (join-thread th))
    (is (queue-empty-p q))))

(deftest cons-queue ()
  (let ((q (make-queue)))
    (istype 'cons-queue q)
    (istype 'list (data q))
    (is (queue-empty-p* q))
    (dotimes (i 10)
      (push-queue i q)
      (is= (pop-queue q) i))
    (is (queue-empty-p q))))

(deftest spin-queue ()
  (let ((q (make-spin-queue)))
    (istype 'spin-queue q)
    (dotimes (i 100)
      (push-spin-queue i q)
      (is= 1 (spin-queue-count q))
      (is= i (pop-spin-queue q)))
    (is (spin-queue-empty-p q))))

(deftest priority-queue ()
  (let ((q (make-queue :prioritize t)))
    (istype 'priority-queue q)))

(deftest fib-heap ()
  (let ((fib (make-heap)))
    (loop for i from 0 below 100 do (fib-insert i fib))
    (loop for i from 0 below 100 
          do (let ((min (extract-min fib)))
	       (iseql i min)))))

(deftest accumulator ()
  (let ((acc (make-instance 'max-accumulator)))
    (istype 'accumulator acc)
    (accumulate acc 40)
    (accumulate acc 32)
    (is= (accumulated acc) 40)
    (accumulate acc 42)
    (is= (accumulated acc) 42)))

(deftest iter ()
  (with-iter (it (make-instance 'iterator))
    (iszero (idx it)))
  (with-iter (it (iota 20))
    seek-to-first
    (iszero next)
    (is= 1 next)
    (iszero prev)
    (is= 13 (seek it 13) idx)
    (is= 8 (seek-for-prev *iter* 8) idx)
    (is iter-valid-p)
    seek-to-last
    (is= idx (1- (length it)))))

(deftest pqueue ()
  (let ((q (make-pqueue)))
    (is (pqueue-empty-p q))
    (pqueue-insert q 40)
    (pqueue-insert q 10)
    (pqueue-insert q 30)
    (pqueue-insert q 20)
    (isequalp #(10 20 30 40) (pqueue-reorder q))
    ;; REVIEW 2026-03-13: no idea why iolib:priority-queue-extract-maximum uses '<=
    (is= 10 (pqueue-extract-maximum q))
    (isequalp #(20 40 30) (data q))
    (is= 2 (pqueue-remove q 30))
    (is= 20 (pqueue-maximum q))))

