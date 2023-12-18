(require :sb-sprof)
(defpackage :sxp/bench
  (:use :cl :sxp :sb-ext :sb-unix)
  (:export :run-bench :*bench-input-file* :*bench-input-string* :*bench-input-object*
	   :*bench-output-directory* :*bench-iterations* :*bench-report-file* ;:*bench-flamegraph-file*
	   ))
(in-package :sxp/bench)
(declaim
 (type (or string pathname) *bench-input-file* *bench-output-directory* *bench-report-file*)
 (type string *bench-input-string*)
 (type sxp *bench-input-object*)
 (type integer *bench-iterations*))
(defvar *bench-input-file* "lisp/lib/sxp/tests.sxp")
(defparameter *bench-input-string* (uiop:read-file-string *bench-input-file*))
(defparameter *bench-input-object* (make-instance 'sxp))
(read-sxp-string *bench-input-object* *bench-input-string*)

(defparameter *bench-output-directory* "/tmp/sxp-bench")
(defparameter *bench-iterations* 1000)
(defparameter *bench-report-file* "bench.sxp")
;; (defparameter *bench-flamegraph-file* "bench.stack")
(defmacro bench (&body body)
  `(loop for i from 1 to *bench-iterations*
	 do ,@body))

(defun rbench (fn input)
  (let ((res))
    (bench (call-with-timing (lambda (&rest x) (push (cons i x) res)) fn input))
    (nreverse res)))

(defun wbench (fn)
  (let ((res))
    (bench (let ((out (make-pathname :name (format nil "~d.sxp" i) :directory *bench-output-directory*)))
	     (call-with-timing (lambda (&rest x) (push (cons i x) res)) fn *bench-input-object* out :if-exists :supersede)))
    (nreverse res)))

(defun run-bench (&optional rpt)
  (when (probe-file *bench-output-directory*)
    (sb-ext:delete-directory *bench-output-directory* :recursive t))
  (sb-unix:unix-mkdir *bench-output-directory* #o777)
  (let ((rres (sb-sprof:with-profiling (:sample-interval 0.001) (rbench #'sxp:read-sxp-file *bench-input-file*)))
	(wres (sb-sprof:with-profiling (:sample-interval 0.001) (wbench #'sxp:write-sxp-file))))
    (if rpt
	(progn
	  (format t "Writing output to ~s" *bench-report-file*)
	  (uiop:with-output-file (out *bench-report-file* :if-exists :supersede :if-does-not-exist :create)
	    (print `(,@rres ,@wres) out)))
	(print (list rres wres))))
  (terpri))
