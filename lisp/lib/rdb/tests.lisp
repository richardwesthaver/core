(defpackage :rdb/tests
  (:use :cl :std :std/alien :std/rt :rdb :rocksdb))
(in-package :rdb/tests)
(defsuite :rdb)
(in-suite :rdb)

(rocksdb:load-rocksdb)

(defmacro with-test-db-raw ((db-var path) &body body)
  `(let* ((opt (rdb::default-rocksdb-options))
          (,db-var (open-db-raw ,path opt)))
     (unwind-protect (progn ,@body)
       (with-errptr e
         (rocksdb-close ,db-var)
         (rocksdb-destroy-db opt ,path e)))))

(deftest rdb ()
  "Test RDB struct and methods."
  (let ((db (make-rdb :name "/tmp/rdb" :opts (make-rdb-opts :create-if-missing t))))
    (open-rdb db)
    (put-kv-str-raw (rdb-db db) "key" "val")
    (is (equal (get-kv-str-raw (rdb-db db) "key") "val"))
    (let ((cfs (list (make-rdb-cf :name "foo") (make-rdb-cf :name "bar") (make-rdb-cf :name "baz"))))
      (dolist (cf cfs)
        (push-cf cf db)))
    (init-cfs db)
    (loop for cf across (rdb-cfs db)
          do
             (progn
               (insert-kv-str db "key" "val" :cf (rdb-cf-name cf))
               (is (equal (get-cf-str-raw (rdb-db db) (rdb-cf-sap cf) "key") "val"))))
    (rocksdb:rocksdb-cancel-all-background-work (rdb-db db) t)
    (insert-kv-str db "test" "zaa")
    ;; cleanup
    (close-rdb db)
    (destroy-rdb db)))

(deftest with-db-raw ()
  "Test the WITH-OPEN-DB macro and some basic functions."
  (with-test-db-raw (db "/tmp/with-rdb")
    (dotimes (i 10000)
      (let ((k (format nil "key~d" i))
            (v (format nil "val~d" i)))
        (put-kv-str-raw db k v)))))

(deftest with-iter ()
  "Test the WITH-ITER macro."
  (let ((ro (rocksdb:rocksdb-readoptions-create)))
    (with-test-db-raw (db "/tmp/rdb-with-iter")
      (put-kv-str-raw db "ak" "av")
      (put-kv-str-raw db "bk" "bv")
      (rocksdb:rocksdb-cancel-all-background-work db t)
      (is (equal (get-kv-str-raw db "ak") "av"))
      (is (equal (get-kv-str-raw db "bk") "bv"))
      (with-iter (it db ro)
        (rocksdb:rocksdb-iter-seek-to-first it)
        (is (rocksdb:rocksdb-iter-valid it))
        (is (equal (iter-key-str it) "ak"))
        (is (equal (get-kv-str-raw db "ak") "av"))
        (rocksdb:rocksdb-iter-next it)
        (is (rocksdb:rocksdb-iter-valid it))
        (is (equal (iter-key-str it) "bk"))
        (is (equal (iter-val-str it) "bv"))
        (rocksdb:rocksdb-iter-next it)
        (is (not (rocksdb:rocksdb-iter-valid it)))))))

(deftest with-cf ()
  "Test rdb-cf operations"
  (with-open-db-raw (db "/tmp/rdb-with-cf")
    (with-cf (cf (make-rdb-cf :name "foobar"))
      (is (create-cf db cf))
      (is (null (put-cf-str-raw db (rdb-cf-sap cf) "key" "val")))
      (is (equal (get-cf-str-raw db (rdb-cf-sap cf) "key") "val")))))

(deftest errors ()
  "Test rdb condition handlers.")
