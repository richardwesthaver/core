;;; lib/obj/hash/hasher.lisp --- Hash Functions

;;

;;; Code:
(in-package :obj/hash)

(macrolet ((specialize (str body)       ; TODO 2023-12-21: test if this actually compiles to fastpath
             `(if (typep ,str '(simple-array character 1))
                  ,body
                  ,body)))
  (defun djb (string)
    (declare (string string)
             (optimize speed))
    (let ((hash 5381))
      (declare ((and unsigned-byte fixnum) hash))
      (specialize
       string
       (dotimes (n (min 6 (length string)))
         (setf hash
               (logand most-positive-fixnum
                       (logxor (* hash 33)
                               (char-code (schar string n)))))))
      hash)))
