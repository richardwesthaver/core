(defpackage :rdb/tests
  (:use :cl :std :rt :rocksdb :rdb :sb-ext :sb-alien))

(in-package :rdb/tests)

(defsuite :rdb)
(in-suite :rdb)

(rocksdb:load-rocksdb)

(deftest minimal ()
  "Test minimal functionality (open/close/put/get)."
  (let ((db-path (format nil "/tmp/rdb-minimal-~a" (gensym))))
    (with-db (db (open-db-raw db-path))
      (put-kv-str-raw db "foo" "bar")
      (is (string= (get-kv-str-raw db "foo") "bar"))
      (rocksdb-close db)
      (destroy-db-raw db-path))))

(deftest raw ()
  "Test the raw RocksDB function wrappers."
  (let ((path "/tmp/rdb-raw/"))
    (with-open-db-raw (db path)
      (dotimes (i 1000)
        (let ((k (format nil "key~d" i))
              (v (format nil "val~d" i)))
          (put-kv-str-raw db k v)
          (is (string= (get-kv-str-raw db k) v))))
      (let ((cf (create-cf-raw db "cf1")))
        (put-cf-str-raw db cf "bow" "wow")
        (is (string= (get-cf-str-raw db cf "bow") "wow")))
      (with-iter-raw (iter db)
        (rocksdb:rocksdb-iter-seek-to-first iter)
        (dotimes (i 999)
          (rocksdb:rocksdb-iter-next iter)
          (is (rocksdb:rocksdb-iter-valid iter))
          (is (string= (get-kv-str-raw db (iter-key-str-raw iter)) (iter-val-str-raw iter))))
        (rocksdb:rocksdb-iter-next iter)
        (is (not (rocksdb:rocksdb-iter-valid iter)))))
    (destroy-db-raw path)))

(deftest rdb ()
  "Test RDB struct and methods."
  (let ((db (create-db "/tmp/rdb/")))
    (put-kv-str-raw (rdb-db db) "key" "val")
    (is (equal (get-kv-str-raw (rdb-db db) "key") "val"))
    (let ((cfs (list (make-rdb-cf :name "foo") (make-rdb-cf :name "bar") (make-rdb-cf :name "baz"))))
      (dolist (cf cfs)
        (push-cf cf db)))
    (init-db db)
    ;; TODO
    ;; (loop for cf across (rdb-cfs db)
    ;;       do
    ;;          (progn
    ;;            (insert-kv db (make-rdb-kv "key" "val") :cf (rdb-cf-name cf))
    ;;            (is (equal (get-cf-str-raw (rdb-db db) (rdb-cf-sap cf) "key") "val"))))
    (rocksdb:rocksdb-cancel-all-background-work (rdb-db db) t)
    ;; (insert-kv-str db "test" "zaa")
    ;; cleanup
    (close-db db)
    (destroy-db db)))

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
