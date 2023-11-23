(defpackage :rdb/tests
  (:use :cl :std/base :std/alien :rt :rdb))
(in-package :rdb/tests)
(defsuite :rdb)
(in-suite :rdb)

(deftest with-db ()
  "Test the WITH-OPEN-DB macro and some basic functions."
  (with-open-db (db "/tmp/rdb.with-db-test" (make-rdb-opts :create-if-missing t :destroy t))
    (dotimes (i 10000)
      (let ((k (format nil "key~d" i))
            (v (format nil "val~d" i)))
        (put-kv-str db k v)))))

(deftest with-iter ()
  "Test the WITH-ITER macro.")
