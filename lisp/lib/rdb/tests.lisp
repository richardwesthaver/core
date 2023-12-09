(defpackage :rdb/tests
  (:use :cl :std :std/alien :std/rt :rdb))
(in-package :rdb/tests)
(defsuite :rdb)
(in-suite :rdb)
(rocksdb:load-rocksdb)

(deftest rdb (:persist t)
  "Test RDB struct and methods."
  (let ((db (make-rdb :name "/tmp/rdb" :opts (make-rdb-opts :create-if-missing t :destroy t))))
    (open-rdb db)
    (put-kv-str (rdb-db db) "key" "val")
    (is (equal (get-kv-str (rdb-db db) "key") "val"))
    (let ((cfs (list (make-rdb-cf :name "foo") (make-rdb-cf :name "bar") (make-rdb-cf :name "baz"))))
      (dolist (cf cfs)
        (push-cf cf db)))
    (init-cfs db)
    (loop for cf across (rdb-cfs db)
          do
             (progn
               (put-cf-str (rdb-db db) (rdb-cf-sap cf) "key" "val")
               (is (equal (get-cf-str (rdb-db db) (rdb-cf-sap cf) "key") "val"))))
    (rocksdb:rocksdb-cancel-all-background-work (rdb-db db) t)
    (insert-kv-str db "test" "zaa")
    ;; cleanup
    (close-rdb db)
    (destroy-rdb db)))

(deftest with-db ()
  "Test the WITH-OPEN-DB macro and some basic functions."
  (with-open-db (db "/tmp/with-rdb" (make-rdb-opts :create-if-missing t :destroy t))
    (dotimes (i 10000)
      (let ((k (format nil "key~d" i))
            (v (format nil "val~d" i)))
        (put-kv-str db k v)))))

(deftest with-iter ()
  "Test the WITH-ITER macro."
  (let ((ro (rocksdb:rocksdb-readoptions-create)))
    (with-open-db (db "/tmp/rdb-with-iter" (make-rdb-opts :create-if-missing t :destroy t))
      (put-kv-str db "ak" "av")
      (put-kv-str db "bk" "bv")
      (rocksdb:rocksdb-cancel-all-background-work db t)
      (is (equal (get-kv-str db "ak") "av"))
      (is (equal (get-kv-str db "bk") "bv"))
      (with-iter (it db ro)
        (rocksdb:rocksdb-iter-seek-to-first it)
        (is (rocksdb:rocksdb-iter-valid it))
        (is (equal (iter-key-str it) "ak"))
        (is (equal (get-kv-str db "ak") "av"))
        (rocksdb:rocksdb-iter-next it)
        (is (rocksdb:rocksdb-iter-valid it))
        (is (equal (iter-key-str it) "bk"))
        (is (equal (iter-val-str it) "bv"))
        (rocksdb:rocksdb-iter-next it)
        (is (not (rocksdb:rocksdb-iter-valid it)))))))

(deftest with-cf ()
  "Test rdb-cf operations"
  (with-open-db (db "/tmp/rdb-with-cf" (make-rdb-opts :create-if-missing t :destroy t))
    (with-cf (cf (make-rdb-cf :name "foobar"))
      (is (create-cf db cf))
      (is (null (put-cf-str db (rdb-cf-sap cf) "key" "val")))
      (is (equal (get-cf-str db (rdb-cf-sap cf) "key") "val")))))
