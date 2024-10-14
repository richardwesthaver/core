(defpackage :log/tests
  (:use :cl :std :rt :log))

(in-package :log/tests)

(defsuite :log)
(in-suite :log)

(deftest log ()
  "Test logging features"
  (is (debug! "test" *log-level*)))

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
