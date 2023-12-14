(defpackage :rdb/tests
  (:use :cl :std :rt :rocksdb :rdb :sb-ext :sb-alien))

(in-package :rdb/tests)

(defsuite :rdb)
(in-suite :rdb)

(rocksdb:load-rocksdb)

(defun test-cleanup (db opt path)
  (with-errptr err
    (rocksdb-close db)
    (rocksdb-destroy-db opt path err)))

(deftest rdb ()
  "Test RDB struct and methods."
  (let ((db (make-rdb :name "/tmp/rdb" :opts (make-rdb-opts :create-if-missing t))))
    (open-db db)
    (put-kv-str-raw (rdb-db db) "key" "val")
    (is (equal (get-kv-str-raw (rdb-db db) "key") "val"))
    (let ((cfs (list (make-rdb-cf :name "foo") (make-rdb-cf :name "bar") (make-rdb-cf :name "baz"))))
      (dolist (cf cfs)
        (push-cf cf db)))
    (init-db db)
    (loop for cf across (rdb-cfs db)
          do
             (progn
               (insert-kv db (make-rdb-kv "key" "val") :cf (rdb-cf-name cf))
               (is (equal (get-cf-str-raw (rdb-db db) (rdb-cf-sap cf) "key") "val"))))
    (rocksdb:rocksdb-cancel-all-background-work (rdb-db db) t)
    ;; (insert-kv-str db "test" "zaa")
    ;; cleanup
    (close-db db)
    (destroy-db db)))

(deftest with-db-raw ()
  "Test the WITH-OPEN-DB macro and some basic functions."
  (let ((path "/tmp/with-db-raw")
        (opt (default-rocksdb-options)))
    (with-open-db-raw (db path)
    (dotimes (i 10000)
      (let ((k (format nil "key~d" i))
            (v (format nil "val~d" i)))
        (put-kv-str-raw db k v)))
    (test-cleanup db opt path))))

(deftest with-iter ()
  "Test the WITH-ITER macro."
  (let ((ro (rocksdb:rocksdb-readoptions-create)))
    (with-open-db-raw (db "/tmp/rdb-with-iter")
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

(deftest rdb-bytes ()
  "Test rdb-bytes methods - iterator protocol specifically."
  (let ((bytes (make-instance 'rdb-bytes :buffer #(0 1 2 3))))
    (is (= (sequence:length bytes) 4))
    (is (= (sequence:elt bytes 0) 0))
    (is (not (sequence:emptyp bytes)))
    ;; NYI
    (is (= (sequence:count 2 (rdb-bytes-buffer bytes)) 1))))
  
