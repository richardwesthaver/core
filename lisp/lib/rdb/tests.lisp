(defpackage :rdb/tests
  (:use :cl :std :rt :rocksdb :rdb :sb-ext :sb-alien :log :obj :query :db :schema :store)
  (:import-from :rdb :open-db-raw :get-kv-str-raw :iter-key-str-raw
   :destroy-db-raw :close-db-raw :create-cf-raw :get-cf-str-raw
   :iter-val-str-raw :put-kv-str-raw :put-cf-str-raw))

(in-package :rdb/tests)

(defsuite :rdb)
(in-suite :rdb)
(setq rt:*compile-tests* nil)
(load-database-backend :rdb)
(setq *temp-db-destroy* t)

(defmacro with-temp-db ((sym &rest opts) &body body)
  `(with-db (,sym :db (make-db :rdb :name (namestring (tmpize-pathname "/tmp/rdb"))) ,@opts)
     ,@body))

(deftest minimal ()
  "Test minimal functionality (open/close/put/get)."
  (let ((db-path (format nil "/tmp/rdb-minimal-~a" (gensym))))
    (with-rdb (db (open-db-raw db-path))
      (put-kv-str-raw db "foo" "bar")
      (is (string= (get-kv-str-raw db "foo") "bar"))
      (close-db-raw db)
      (destroy-db-raw db-path))))

(deftest opts ()
  "Ensure RDB-OPTS can be created, destructured, etc."
  (let ((default (default-rdb-opts)))
    ;; check defaults
    (is (< 50 (hash-table-count (backfill-opts default :full t))))
    (is (typep (sap default) '(alien (* rocksdb-options))))
    (is (eql t (db-opt default "create-if-missing")))
    (is (eql t (set-db-opt default "enable-blob-files" t :push t)))
    (is (eql t (db-opt default "enable-blob-files")))
    (is (eql t (rocksdb-options-get-enable-blob-files (sap default))))
    (is (null (rocksdb-options-get-error-if-exists (sap default))))))

(deftest raw ()
  "Test the raw RocksDB function wrappers."
  (let ((path (merge-pathnames (symbol-name (gensym "rdb-raw")) "/tmp/")))
    (with-open-rdb-raw (db path)
      (dotimes (i 1000)
        (let ((k (format nil "key~d" i))
              (v (format nil "val~d" i)))
          (put-kv-str-raw db k v)
          (is (string= (get-kv-str-raw db k) v))))
      (let ((cf (create-cf-raw db "cf1")))
        (put-cf-str-raw db cf "bow" "wow")
        (is (string= (get-cf-str-raw db cf "bow") "wow"))))
    (destroy-db-raw path)))

(deftest temp-db ()
  "Test WITH-TEMP-DB macro."
  (with-temp-db (tmp :open nil :destroy t)
    (set-db-opt tmp :parallelism (num-cpus))
    ;; https://github.com/facebook/rocksdb/wiki/unordered_write
    (set-db-opt tmp :unordered-write t)
    (set-db-opt tmp :enable-statistics t)
    (set-db-opt tmp :statistics-level (rocksdb-statistics-level "all"))
    (push-opts tmp)
    (open-db tmp)
    (create-columns tmp)
    (with-iter (it (iter tmp))
      (seek-to-first it)
      (is (sequence:emptyp (key it)))
      (is (sequence:emptyp (val it)))
      (is (zerop (nth 1 (multiple-value-list (timestamp it)))))
      (is (not (iter-valid-p it)))
      (seek-to-last it)
      (is (typep (kv it) 'kv))
      (is (sequence:emptyp (key it)))
      (is (sequence:emptyp (val it)))
      ;; (info! (iter-next it))
      (rocksdb-iter-destroy (sap it)))
    (dotimes (i 10000)
      (insert-key tmp (format nil "foo~A" i) (format nil "bar~A" i)))
    (loop for i below 100
          with n = (* i i)
          do (is (string= (get-val tmp (format nil "foo~A" n)) (format nil "bar~A" n))))
    (flush-db tmp)
    ;; TODO: auto handle return type (get-prop-int)
    (is (= 10000 (parse-integer (db-prop tmp "rocksdb.estimate-num-keys"))))
    (istype 'string (print-stats tmp))
    (istype 'string (db-prop tmp :levelstats))
    (debug! ;; some info about our db
     (name tmp)
     (db-prop tmp "rocksdb.dbstats"))))

(deftest metadata ()
  "Test metadata types: CF -> LEVEL -> SST-FILE."
  (with-temp-db (tmp :open t :close t)
    (insert-key tmp "foo" "bar")
    (flush-db tmp)
    (let ((cf-meta (db-metadata tmp)))
      (is (rdb-cf-metadata-p (pull-sap* cf-meta)))
      (let ((level-meta (db-metadata cf-meta)))
        (is (rdb-level-metadata-p (pull-sap* level-meta)))
        (is (rdb-sst-file-metadata-p
             (pull-sap* (db-metadata level-meta))))))))

(deftest sst ()
  "Test SST-FILE-WRITER and INGEST-DB."
  (with-temp-db (tmp :open t :close t)
    ;; without macro
    (let ((writer (make-sst-file-writer))
          (path (format nil "/tmp/~A" (gensym "sst"))))
      (open-sst writer path)
      (dotimes (i 10000)
        (put-key writer (integer-to-octets i 64) (string-to-octets (format nil "~A" (gensym)))))
      (finish-sst writer) ;; will fail on empty writer
      (destroy-sst writer)
      (ingest-db tmp (list path))
      (delete-file path)
      (with-sst (s :file path)
        (put-kv s (make-kv (string-to-octets "nil") (string-to-octets "nil"))))
      (delete-file path))))

(deftest schema ()
  "Test loading and handling of RDB-SCHEMA objects."
  (let ((cf (load-field (make-instance 'rdb-column-family
                          :cf (make-rdb-cf "foo"))
                        (make-field :type '(string string)))))
    (isequal (column-type cf) (cons 'string 'string))
    (isequal (name cf) "foo"))
  (with-temp-db (db :destroy t :open t)
    (load-schema db (make-simple-schema (make-field :type nil)))
    (is (= 1 (length (columns db)))))
  (with-temp-db (db1 :open t :destroy t)
    (load-schema db1 (make-simple-schema (make-field :name "BAZ" :type '(octet-vector . string))))
    (is (= 1 (length (columns db1))))))

(deftest transaction ()
  "Test OBJ/DB transactions."
  (with-db (db :db (make-db :rdb :name (format nil "/tmp/~A" (random-chars 4)) :columns nil)
               :open t
               :destroy t)
    (open-transaction-db db :path (format nil "/tmp/~A" (random-chars 4))
                            :opts (rocksdb-transactiondb-options-create))
    (istype 'rdb-transaction-db (transaction-db db))
    (let ((txn1 (make-transaction db)))
      (isnt (abort-transaction txn1)))
    (let ((txn2 (make-transaction db :name "foofn" :optimistic t)))
      (prepare-transaction txn2)
      (rocksdb-transaction-set-savepoint (sap txn2))
      (isequal (name txn2) "foofn")
      (rocksdb-transaction-destroy (sap txn2)))
    (with-transaction (txn :db db)
      (istype 'rdb-transaction txn))))

(deftest merge-op ()
  "Test custom RocksDB merge operator."
  (let ((k "foo")
        (v "bar"))
    (with-db (db :db (make-db :rdb
                              :name (format nil "/tmp/~A" (random-chars 4))
                              :merge-op (rdb::create-concat-merge-op))
                 :open t :close t)
      (put-key db k v)
      (merge-key db k v)
      (isequal (concatenate 'string v v) (get-val db k)))))

(deftest prefix-op ()
  "Test custom RocksDB prefix extractor."
  (let ((k "1337gamer")
        (v "foobarbaz"))
    (with-db (db :db (make-db :rdb
                              :name (format nil "/tmp/~A" (random-chars 4))
                              :prefix-op (create-fixed-prefix-op 4))
                 :open t :close t)
      (put-key db k v))))

(deftest store ()
  (with-store (store)))

(deftest logger ()
  (with-db (db :db (make-db :rdb
                            :name (format nil "/tmp/~A" (random-chars 4))
                            :logger (create-default-logger-callback))
               :open nil 
               :close t)
    (open-db db)))
          
(deftest wbwi ()
  (with-wbwi (wbwi)
    (is wbwi)
    (iszero (rdb-wbwi-count wbwi))
    (put-key wbwi "foo" "bar")
    (isequal "bar" (get-key wbwi "foo"))
    (is= 1 (rdb-wbwi-count wbwi))
    (rdb-wbwi-clear wbwi)))
