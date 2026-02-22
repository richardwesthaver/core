;;; fuzz.lisp --- RT Fuzz

;; FUZZER API

;;; Commentary:

;; 
;; wiki: https://en.wikipedia.org/wiki/Fuzzing

;;; Code:
(in-package :rt/fuzz)

(defvar *default-fuzz-generator*
  (lambda (state)
    (random most-positive-fixnum state)))

(defkernel fuzzer ()
  ((state :initform (make-random-state t)
          :initarg :state :accessor state))
  (:documentation "An object which provides invalid, unexpected or random data as inputs to some
program.")
  (:kernel *default-fuzz-generator*))

(definline fuzzer (&optional (generator *default-fuzz-generator*) (state (make-random-state t)))
  (let ((f (make-instance 'fuzzer :state state)))
    (set-funcallable-instance-function f generator)
    f))

(defgeneric fuzz (self &key &allow-other-keys)
  (:method ((self fuzzer) &key &allow-other-keys)
    (funcall (the function (kernel self)) (state self)))
  (:method ((self fuzzer) &key count)
    (if count
        (let ((ret))
          (dotimes (i count ret)
            (push (funcall (the function (kernel self)) (state self)) ret)))
        (fuzz self))))

(defgeneric fuzz* (state generator &key &allow-other-keys)
  (:method ((state list) (generator function) &key (count 1))
    (let ((ret))
      (dotimes (i count ret)
        (push (funcall generator state) ret))))
  (:method ((state vector) (generator function) &key (count 1))
    (let ((ret (make-array count :fill-pointer 0)))
      (dotimes (i count ret)
        (setf (aref ret i) (funcall generator state)))))
  (:method ((state hash-table) (generator function) &key (count 1))
    (let ((ret (make-hash-table)))
      (dotimes (i count ret)
        (destructuring-bind (k v) (funcall generator state)
          (setf (gethash k ret) v)))))
  (:method ((state random-state) (generator function) &key (count 1))
    (let ((ret))
      (dotimes (i count ret)
        (push (funcall generator state) ret)))))
