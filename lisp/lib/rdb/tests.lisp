(defpackage :rdb/tests
  (:use :cl :std :rt :rocksdb :rdb :sb-ext :sb-alien :log))

(in-package :rdb/tests)

(defsuite :rdb)
(in-suite :rdb)

(rocksdb:load-rocksdb)
(setq *temp-db-destroy* t)

(deftest minimal ()
  "Test minimal functionality (open/close/put/get)."
  (let ((db-path (format nil "/tmp/rdb-minimal-~a" (gensym))))
    (with-db (db (open-db-raw db-path))
      (put-kv-str-raw db "foo" "bar")
      (is (string= (get-kv-str-raw db "foo") "bar"))
      (close-db-raw db)
      (destroy-db-raw db-path))))

(deftest opts ()
  "Ensure RDB-OPTS can be created, destructured, etc."
  (let ((default (default-rdb-opts)))
    ;; check defaults
    (is (< 100 (hash-table-size (backfill-opts default))))
    (is (typep (rdb-opts-sap default) '(alien (* rocksdb-options))))
    (is (eql t (get-opt default "create-if-missing")))
    (is (eql t (set-opt default "enable-blob-files" t :push t)))
    (is (eql t (get-opt default "enable-blob-files")))
    (is (eql t (rocksdb-options-get-enable-blob-files (rdb-opts-sap default))))
    (is (null (rocksdb-options-get-error-if-exists (rdb-opts-sap default))))))

(deftest raw ()
  "Test the raw RocksDB function wrappers."
  (let ((path (merge-pathnames (symbol-name (gensym "rdb-raw")) "/tmp/")))
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
  ;; NOTE: passing a directory with trailing slash causes segfault - guess we gotta handle tht
  (with-db (db (debug! (create-db "/tmp/rdb" :open t)))
    (info! (hash-table-alist (backfill-opts db :full t)))
    ;; get/set without cf
    (put-kv-str-raw (rdb-db db) "key" "val")
    (is (equal (get-kv-str-raw (rdb-db db) "key") "val"))
    ;; push 3 cfs
    (let ((cfs (list (make-rdb-cf "foo") (make-rdb-cf "bar") (make-rdb-cf "baz"))))
      (dolist (cf cfs)
        (push-cf cf db)))
    (debug! (rdb-cfs db))
    (create-cfs db)
    ;; TODO
    (do-cfs (cf (rdb-cfs db))
      (insert-kv db (make-kv "key" "val") :cf cf)
      (is (equal (get-key db "key" :cf (rdb-cf-sap cf)) "val")))
    (rocksdb-cancel-all-background-work (rdb-db db) nil)
    ;; insert after background cancel
    (insert-key db "test" "zaa")
    (is (string= "zaa" (get-key db "test")))
    ;; cleanup
    (destroy-db db)))

(deftest temp-db ()
  "Test WITH-TEMP-DB macro."
   (with-temp-db (tmp (cf1 cf2 cf3 cf4) :destroy t)
     (set-opt tmp :parallelism (num-cpus))
     ;; https://github.com/facebook/rocksdb/wiki/unordered_write
     (set-opt tmp :unordered-write t)
     (set-opt tmp :enable-statistics t)
     (set-opt tmp :statistics-level (rocksdb-statistics-level "all"))
     (push-opts tmp)
     (open-db tmp)
     (create-cfs tmp)
     (with-iter (it (create-iter tmp))
       (iter-seek-to-first it)
       (is (sequence:emptyp (iter-key it)))
       (is (sequence:emptyp (iter-val it)))
       (is (iter-valid-p it))
       (iter-seek-to-last it)
       (is (typep (iter-kv it) 'rdb-kv))
       (is (sequence:emptyp (iter-key it)))
       (is (sequence:emptyp (iter-val it)))
       ;; (info! (iter-next it))
       (rocksdb-iter-destroy (rdb-iter-sap it)))
     (dotimes (i 10000)
       (insert-key tmp (format nil "foo~A" i) (format nil "bar~A" i)))
     (loop for i below 100
           with n = (* i i)
           do (is (string= (get-key tmp (format nil "foo~A" n)) (format nil "bar~A" n))))
     (flush-db tmp)
     ;; TODO: auto handle return type (get-prop-int)
     (is (= 10000 (parse-integer (get-prop tmp "rocksdb.estimate-num-keys"))))
     (debug! ;; some info about our db
      (rdb-name tmp)
      (get-prop tmp "rocksdb.dbstats")
      (get-prop tmp "rocksdb.levelstats")
      (print-stats tmp)
      )))

(deftest metadata ()
  "Test metadata types: CF -> LEVEL -> SST-FILE."
  (with-temp-db (tmp () :open t)
    (insert-key tmp "foo" "bar")
    (flush-db tmp)
    (let ((cf-meta (get-metadata tmp)))
      (is (rdb-cf-metadata-p (pull-sap* cf-meta)))
      (let ((level-meta (get-metadata cf-meta)))
        (is (rdb-level-metadata-p (pull-sap* level-meta)))
        (is (rdb-sst-file-metadata-p
             (pull-sap* (get-metadata level-meta))))))))
