;;; os-prng.lisp --- OS-provided pseudo-random number generator

;;; Code:
(in-package :crypto)

#+unix
(defparameter *os-prng-stream* nil)
#+unix
(defparameter *os-prng-stream-lock* (std:make-mutex))

(defclass os-prng ()
  ())

(defmethod prng-random-data (num-bytes (prng os-prng))
  #+unix
  (let* ((seq (make-array num-bytes :element-type '(unsigned-byte 8)))
         (n (sb-thread:with-mutex (*os-prng-stream-lock*)
              (unless (and *os-prng-stream* (open-stream-p *os-prng-stream*))
                (setf *os-prng-stream* (open #P"/dev/urandom"
                                             :element-type '(unsigned-byte 8))))
              (read-sequence seq *os-prng-stream*))))
    (if (< n num-bytes)
        (error 'ironclad-error :format-control "Failed to get random data.")
        seq))
  #-unix
  (error 'ironclad-error
         :format-control "OS-RANDOM-SEED is not supported on your platform."))

(defmethod make-prng ((name (eql :os)) &key seed)
  (declare (ignorable seed))
  (make-instance 'os-prng))

(setf *prng* (make-prng :os))

#+thread-support
(pushnew '(*prng* . (make-prng :os)) std:*default-special-bindings* :test #'equal)
