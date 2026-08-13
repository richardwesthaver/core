;;; tests.lisp --- RDB Tests

;; 

;;; Code:
(defpkg :rdb/tests
  (:use :cl :std :rt :rocksdb :rdb :sb-ext :sb-alien :log :obj :q :db :schema :store)
  (:import-from :rdb :%open-db :%get-kv-str
   :%destroy-db :%close-db :%create-cf :%get-cf-str
   :%iter-val-str :%put-kv-str :%put-cf-str :%iter-key-str))

(in-package :rdb/tests)

(defsuite :rdb)
(in-suite :rdb)
(load-alien :rocksdb)
(load-database-backend :rdb)
(setq *temp-db-destroy* t)

(defmacro with-temp-db ((sym &rest opts) &body body)
  `(with-db (,sym :db (make-db :rdb :name (namestring (tmpize-pathname "/tmp/rdb"))) ,@opts)
     ,@body))

(defvar *rdb-schema-file* #P"test.rdb")
(defvar *rdb-schema-file-contents* 
  "; RDB schema file
")

(deftest minimal ()
  "Test minimal functionality (open/close/put/get)."
  (let ((db-path (format nil "/tmp/rdb-minimal-~a" (gensym))))
    (with-rdb (db (%open-db db-path))
      (%put-kv-str db "foo" "bar")
      (is (string= (%get-kv-str db "foo") "bar"))
      (%close-db db)
      (%destroy-db db-path))))

(deftest opts ()
  "Ensure ROCKSDB-OPTIONS can be created, destructured, etc."
  (let ((default (default-rocksdb-options)))
    ;; check defaults
    (is (typep default '(alien (* rocksdb-options))))
    (is (funcall (rdb::rocksdb-options-getter "create-if-missing") default))
    (funcall (rdb::rocksdb-options-setter "enable-blob-files") default t)
    (is (rdb::rocksdb-option "enable-blob-files" default))
    (is (rocksdb-options-get-enable-blob-files default))
    (isnt (rocksdb-options-get-error-if-exists default))))

(deftest raw ()
  "Test the raw RocksDB function wrappers."
  (let ((path (merge-pathnames (symbol-name (gensym "rdb-raw")) "/tmp/")))
    (with-open-rdb-raw (db path)
      (dotimes (i 1000)
        (let ((k (format nil "key~d" i))
              (v (format nil "val~d" i)))
          (%put-kv-str db k v)
          (is (string= (%get-kv-str db k) v))))
      (let ((cf (%create-cf db "cf1")))
        (%put-cf-str db cf "bow" "wow")
        (is (string= (%get-cf-str db cf "bow") "wow"))))
    (%destroy-db path)))

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
    (make-columns tmp)
    (with-iter (it (iter tmp))
      (is (sap it))
      seek-to-first
      (is (sequence:emptyp key))
      (is (sequence:emptyp val))
      (is (zerop (nth 1 (multiple-value-list (timestamp it)))))
      (is (not iter-valid-p))
      seek-to-last
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
    (flush tmp)
    ;; TODO: auto handle return type (get-prop-int)
    (is (= 10000 (parse-integer (db-prop tmp "rocksdb.estimate-num-keys"))))
    (istype 'string (print-stats tmp nil))
    (istype 'string (db-prop tmp :levelstats))))

(deftest metadata ()
  "Test metadata types: CF -> LEVEL -> SST-FILE."
  (with-temp-db (tmp :open t :close t)
    (insert-key tmp "foo" "bar")
    (flush tmp)
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
    (let ((writer (make-sst-file-writer :path (tmp-path "sst"))))
      (open-db writer)
      (dotimes (i 10000)
        (put-key writer (integer-to-octets i 64) (string-to-octets (format nil "~A" (gensym)))))
      (close-db writer) ;; will fail on empty writer
      (shutdown-db writer)
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
    (load-schema db (make-simple-schema :foo (make-field :type nil)))
    (is (= 1 (length (columns db)))))
  (with-temp-db (db1 :open t :destroy t)
    (load-schema db1 (make-simple-schema :bar (make-field :name "BAZ" :type '(octet-vector . string))))
    (is (= 1 (length (columns db1))))))

(deftest transaction ()
  "Test OBJ/DB transactions."
  (with-db (db :db (make-db :rdb-transaction :path (format nil "/tmp/~A" (random-chars 4)))
               :columns nil
               :open t
               :close t
               :destroy t)
    ;; (open-transaction-db db :path (format nil "/tmp/~A" (random-chars 4)))
    (let ((txn1 (transaction db)))
      (isnt (abort-transaction txn1)))
    (let ((txn2 (transaction db :name "foofn" :optimistic t)))
      (prepare txn2)
      (rocksdb-transaction-set-savepoint (sap txn2))
      (isequal (name txn2) "foofn")
      (abort-transaction txn2))
    (with-transaction (:db db :txn (transaction db))
      (print (istype 'rdb-transaction *transaction*)))))

(deftest merge-op ()
  "Test custom RocksDB merge operator."
  (let ((k "foo")
        (v "bar"))
    (with-db (db :db (make-db :rdb
                              :name (format nil "/tmp/~A" (random-chars 4))
                              :merge-op (rdb::create-concat-merge-op))
                 :open t :close t)
      (put-key db k v)
      (get-val db k)
      (merge-key db k v)
      (isequal (concatenate 'string v v) (get-val db k)))))

;; TODO 2025-04-22: 
(deftest prefix-op ()
  "Test custom RocksDB prefix extractor."
  (let ((k "1337gamer")
        (v "foobarbaz"))
    (with-db (db :db (make-db :rdb
                              :name (format nil "/tmp/~A" (random-chars 4))
                              :prefix-op (create-fixed-prefix-op 4))
                 :open t :close t)
      (put-key db k v)
      (get-val db k))))

(deftest store (:skip :todo)
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
    (iszero (rdb::%wbwi-count wbwi))
    (rdb::%wbwi-put-kv-str wbwi "foo" "bar")
    (isequal "bar" (sb-ext:octets-to-string (rdb::%wbwi-kv-str wbwi "foo")))
    (is= 1 (rdb::%wbwi-count wbwi))
    (rdb::%wbwi-clear wbwi)))
