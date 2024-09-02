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

(defclass fuzzer ()
  ((state :initform (make-random-state t)
    :initarg :state
          :accessor fuzz-state)
   (generator :initform *default-fuzz-generator*
              :initarg :generator
              :type function
              :accessor fuzz-generator))
  (:documentation "An object which provides invalid, unexpected or random data as inputs to some
program."))

(defgeneric fuzz (self &key &allow-other-keys)
  (:method ((self fuzzer) &key &allow-other-keys)
    (funcall (the function (fuzz-generator self)) (fuzz-state self)))
  (:method ((self fuzzer) &key count)
    (if count
        (let ((ret))
          (dotimes (i count ret)
            (push (funcall (the function (fuzz-generator self)) (fuzz-state self)) ret)))
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
          (setf (gethash k ret) v))))))
