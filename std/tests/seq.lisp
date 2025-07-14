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
      (is= (push-queue i q) i)
      (is= (pop-queue i q) i))
    (is queue-empty-p* q)))

(deftest spin-queue ()
  (let ((q (make-spin-queue)))
    (istype 'spin-queue q)
    (dotimes (i 100)
      (push-spin-queue i q)
      (is= i (spin-queue-count q))
      (is= i (pop-spin-queue q)))
    (is (spin-queue-empty-p q))))

(deftest priority-queue ()
  (let ((q (make-queue :prioritize t)))
    (istype 'priority-queue q)))

(deftest accumulator ()
  (let ((acc (make-instance 'max-accumulator)))
    (istype 'accumulator acc)
    (accumulate acc 40)
    (accumulate acc 32)
    (is= (accumulated acc) 40)
    (accumulate acc 2)
    (is= (accumulated acc) 40)))

;; TODO 2025-07-14: 
(deftest iter ()
  (with-iter (it (make-instance 'iterator))
    (iszero (idx it))))
