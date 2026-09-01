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
(setq *temp-db-destroy* t)

(defmacro with-temp-db ((sym &rest opts) &body body)
  (with-gensyms (db)
    `(let ((,db (make-db :rdb :path (namestring (tmpize-pathname "/tmp/rdb")))))
       (with-db (,sym :db ,db ,@opts)
         ,@body)
       (destroy-db ,db))))

(defmacro with-open-rdb-raw ((db-var db-path &optional (opt (default-rocksdb-options))) &body body)
  `(let ((,db-var (%open-db ,db-path ,opt)))
     (unwind-protect (progn ,@body)
       (rocksdb-close ,db-var)
       (with-errptr* (err 'open-db-error)
         (rocksdb-options-destroy ,opt)))))

(defvar *rdb-schema-file* #P"test.rdb")
(defvar *rdb-schema-file-contents* 
  "; RDB schema file
")

(deftest minimal ()
  "Test minimal functionality (open/close/put/get)."
  (let ((db-path (format nil "/tmp/rdb-minimal-~a" (gensym))))
    (with-rdb (db (%open-db db-path (default-rocksdb-options)))
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
    (is (rocksdb-options-get-create-if-missing default))
    (is (rdb::rocksdb-option "enable-blob-files" default))
    (is (rocksdb-options-get-enable-blob-files default))
    (isnt (rocksdb-options-get-error-if-exists default))))

(deftest raw ()
  "Test the raw RocksDB function wrappers."
  (with-temp-db (db :open t :close t)
    (dotimes (i 1000)
      (let ((k (format nil "key~d" i))
            (v (format nil "val~d" i)))
        (%put-kv-str (db db) k v)
        (is (string= (%get-kv-str (db db) k) v))))
    (let ((cf (%create-cf (db db) "cf1")))
      (%put-cf-str (db db) cf "bow" "wow")
      (is (string= (%get-cf-str (db db) cf "bow") "wow"))
      (rdb::%destroy-cf cf))))

(deftest temp-db ()
  "Test WITH-TEMP-DB macro."
  (with-temp-db (tmp :open nil :close t)
    ;; https://github.com/facebook/rocksdb/wiki/unordered_write
    (setf (opt tmp :parallelism) (num-cpus)
          ;; (opt tmp :unordered-write) t ; incompatible with pipeline-write
          (opt tmp :enable-statistics) t
          (opt tmp :statistics-level) (rocksdb-statistics-level "all"))
    (open-db tmp)
    (isnt (rdb::open-all-columns tmp))
    (with-rdb-iter (it (iter tmp))
      (is *iter*)
      seek-to-first
      (is (not iter-valid-p))
      seek-to-last
      ;; (info! (iter-next it))
      (rocksdb-iter-destroy it))
    (dotimes (i 10000)
      (insert-key tmp (format nil "foo~A" i) (format nil "bar~A" i)))
    (loop for i below 100
          with n = (* i i)
          do (is (string= (get-val tmp (format nil "foo~A" n)) (format nil "bar~A" n))))
    (flush tmp)
    ;; TODO: auto handle return type (get-prop-int)
    (is (= 10000 (parse-integer (prop tmp "rocksdb.estimate-num-keys"))))
    (istype 'string (print-db-stats tmp nil))
    (istype 'string (prop tmp :levelstats))))

(deftest metadata ()
  "Test metadata types: CF -> LEVEL -> SST-FILE."
  (with-temp-db (tmp :open t :close t)
    (insert-key tmp "foo" "bar")
    (flush tmp)
    (let ((cf-meta (metadata tmp)))
      (is (rdb::rdb-metadata-p cf-meta))
      (let ((level-meta (metadata cf-meta)))
        (is (rdb::level-metadata-p level-meta))
        (is (rdb::sst-file-metadata-p (metadata level-meta)))))))

(deftest sst ()
  "Test SST-FILE-WRITER and INGEST-DB."
  (let ((path (tmp-path "sst")))
    (let ((writer (make-sst-file-writer :path path)))
      (open-db writer)
      (dotimes (i 10000)
        (put-key writer (integer-to-octets i 64) (string-to-octets (format nil "~A" (gensym)))))
      (shutdown-db writer))
    (with-sst (s :file path)
      (rdb::%sst-put s (integer-to-octets 10000 64) (string-to-octets (format nil "~A" (gensym)))))
    (with-temp-db (tmp :open t)
        (ingest-db tmp (list path))
        (shutdown-db tmp :wait t))
    (delete-file path)))

(deftest schema ()
  "Test loading and handling of RDB-SCHEMA objects."
  (let ((cf (load-field (make-instance 'simple-column-family)
                        (make-field :type '(string string)))))
    (isequal (column-type cf) (cons 'string 'string)))
  (with-temp-db (db :open t)
    (is (load-schema db (make-simple-schema :foo (make-field :name :bar :type nil))))
    (is (= 1 (length (columns db))))
    (close-columns db)
    (shutdown-db db :wait t))
  (with-temp-db (db1 :open t)
    (is (load-schema db1 (make-simple-schema :bar (make-field :name "BAZ" :type '(octet-vector . string)))))
    (is (= 1 (length (columns db1))))
    (close-columns db1)
    (shutdown-db db1)))

(deftest transaction ()
  "Test OBJ/DB transactions."
  (with-db (db :db (make-db :trdb :path (format nil "/tmp/~A" (random-chars 4)))
               :open t)
    (let ((txn1 (transaction db)))
      (isnt (abort-transaction txn1)))
    (let ((txn2 (transaction db :name "foofn")))
      (prepare txn2)
      (rocksdb-transaction-set-savepoint txn2)
      (isequal (rdb::%transaction-name txn2) "foofn")
      (abort-transaction txn2))
    (with-transaction (:db db :transaction (transaction db))
      (is *transaction*))
    (shutdown-db db :wait t)
    (destroy-db db)))

(deftest merge-op ()
  "Test custom RocksDB merge operator."
  (let ((k "foo")
        (v "bar"))
    (with-db (db :db (make-db :rdb
                       :path (format nil "/tmp/~A" (random-chars 4))
                       :options (rdb::default-rocksdb-options* :merge-operator (rdb::concat-merge-op)))
                 :open t)
      (put-key db k v)
      (get-val db k)
      (merge-key db k v)
      (isequal (concatenate 'string v v) (get-val db k))
      (shutdown-db db)
      (destroy-db db))))

(deftest prefix-op ()
  "Test custom RocksDB prefix extractor."
  (let ((k "1337gamer")
        (v "foobarbaz"))
    (with-db (db :db (make-db :rdb
                       :path (format nil "/tmp/~A" (random-chars 4))
                       :options (rdb::default-rocksdb-options* :prefix-extractor (rdb::fixed-prefix-op 4)))
                 :open t)
      (put-key db k v)
      (is (string= v (get-val db k)))
      (shutdown-db db :wait t)
      (destroy-db db))))

(deftest srdb ()
  (with-db (db :db (make-db :srdb
                     :path (namestring (tmpize-pathname "/tmp/rdb"))
                     :options (rdb::default-rocksdb-options*
                               :info-log (create-default-logger-callback))))
    (open-db db)
    (shutdown-db db :wait t)
    (destroy-db db)))

(deftest wbwi ()
  (with-wbwi (wbwi)
    (is wbwi)
    (iszero (rdb::%wbwi-count wbwi))
    (rdb::%wbwi-put-kv-str wbwi "foo" "bar")
    (isequal "bar" (rdb::%wbwi-kv-str wbwi "foo"))
    (is= 1 (rdb::%wbwi-count wbwi))
    (rdb::%wbwi-clear wbwi)))
