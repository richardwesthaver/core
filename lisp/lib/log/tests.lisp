(defpackage :log/tests
  (:use :cl :std :rt :log))

(in-package :log/tests)

(eval-always
  (defclass logger-fixture (logger fixture) ()))

(defsuite :log 
  :level :trace
  :fixtures (list (make-instance 'logger-fixture :name "log-test-logger")))

(in-suite :log)

(deftest simple-log-message ()
  "Test a simple LOG-MESSAGE"
  (istype 'string (format-message nil (make-instance 'simple-log-message :content "hi" :tags '(:test))))
  (let ((*logger* (make-instance 'logger)))
    (istype 'string (format-message nil (log-message :error nil "test")))))

(deftest simple-log-PIPE ()
  "Test a simple LOG-PIPE.")

(deftest simple-log ()
  "Test logging features"
  (is (debug! "test" *log-level*))
  (is (info! "test"))
  (is (trace! "test"))
  (is (error! "test"))
  (is (fatal! "test"))
  (is (warn! "test")))

(deftest stream ()
  (let ((str (random-bytes 1024))
        (lock (make-mutex))
        (*tmp* (tmpize-pathname "/tmp/log-stream")))
    (with-log-stream (st *tmp* lock)
      (write-sequence str st))
    (is (= (length str) (file-size *tmp*)))
    (delete-file *tmp*)))

(deftest fast-stream ()
  (let ((str (random-bytes 1024))
        (lock (make-mutex))
        (*tmp* (tmpize-pathname "/tmp/fast-log-stream")))
    (with-fast-log-stream (st *tmp* lock)
      (io/fast:fast-write-sequence str st))
    (is (= (length str) (file-size *tmp*)))
    (delete-file *tmp*)))
