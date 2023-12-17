;;; lib/rt/bench.lisp --- Benchmarking Framework

;; This package provides an interface for benchmarking Lisp code.

;;; Code:
(in-package :rt/bench)

(defvar *bench-count* 100 "Default number of iterations to repeat a bench test for. This value is
used when the slot value of :BENCH is t.")

(defmacro bench (iter &body body)
  `(loop for i from 1 to ,iter
	 do ,@body))

(defmethod do-bench ((self test) &optional fx)
  (declare (ignorable fx))
  (with-test-env self
    (flet ((%do ()
	     (if-let ((opt *compile-tests*))
	       (progn 
		 (when (eq opt t) (setq opt *default-test-opts*))
		 ;; TODO 2023-09-21: handle failures here
		 (let ((fn (compile-test self :declare opt)))
		   (bench *bench-count* (funcall fn)))
		 (setf %test-result (make-test-result :pass (test-fn self))))
	       (progn
		 (bench *bench-count* (eval-test self))
		 (setf %test-result (make-test-result :pass (test-name self)))))))
      (if *catch-test-errors*
	  (handler-bind
	      ((style-warning #'muffle-warning)
	       (error 
		 #'(lambda (c)
		     (setf %test-bail t)
		     (setf %test-result (make-test-result :fail c))
		     (return-from %test-bail %test-result))))
	    (%do))
	  (%do)))))


(defmacro defbench ())
