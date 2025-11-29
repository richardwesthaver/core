;;; pipe.lisp --- Pipe tests

;; 

;;; Code:
(in-package :std/tests)
(in-suite :std)

(deftest basic-pipes ()
  (let* ((p (defpipe* (make-instance 'pipe) '(stream-sink :output nil)))
         (m (make-instance 'simple-message :content "foo")))
    (istype 'vector (pipe p))
    (istype 'simple-message (msg p m))
    (istype 'simple-message (msg p "foo"))
    (loop for x being the element of p do (istype 'element x)))
