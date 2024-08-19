;;; rocksdb/tests.lisp --- RocksDB tests

;;; Code:
(defpackage :rocksdb/tests
  (:use :cl :std :rt :rocksdb :sb-ext :sb-alien :log))

(in-package :rocksdb/tests)

(defsuite :rocksdb)
(in-suite :rocksdb)

(load-rocksdb)
(init-log-timestamp)

(defun rocksdb-test-dir ()
  (format nil "/tmp/~A/" (gensym "rocksdb-tests-")))

(defun rocksdb-test-file ()
  (format nil "/tmp/~A" (gensym "rocksdb-test-")))

(defun test-opts () 
  (let ((default (rocksdb-options-create)))
    (rocksdb-options-set-create-if-missing default t)
    default))

;; not thread safe (gensym-counter)
(defun genkey (&optional prefix) (string-to-octets (symbol-name (gensym (or prefix "key")))))
(defun genval (&optional prefix) (string-to-octets (symbol-name (gensym (or prefix "val")))))

(defmacro with-opt ((var create destroy) &body body)
  `(let ((,var ,create))
     (unwind-protect (progn ,@body)
       ,destroy)))

(deftest opts ()
  (with-opt (o (rocksdb-options-create) (rocksdb-options-destroy o))
    ;; unsigned-char
    (rocksdb-options-set-create-if-missing o t)
    (rocksdb-options-get-create-if-missing o)
    (rocksdb-options-set-create-missing-column-families o t)
    (rocksdb-options-get-create-missing-column-families o)
    (rocksdb-options-set-error-if-exists o t)
    (rocksdb-options-get-error-if-exists o)
    (rocksdb-options-set-paranoid-checks o t)
    (rocksdb-options-get-paranoid-checks o)
    (rocksdb-options-set-compression-options-use-zstd-dict-trainer o t)
    (rocksdb-options-get-compression-options-use-zstd-dict-trainer o)
    (rocksdb-options-set-enable-blob-gc o t)
    (rocksdb-options-get-enable-blob-gc o)
    (rocksdb-options-set-allow-ingest-behind o t)
    (rocksdb-options-get-allow-ingest-behind o)
    (rocksdb-options-set-skip-stats-update-on-db-open o t)
    (rocksdb-options-get-skip-stats-update-on-db-open o)
    (rocksdb-options-set-skip-checking-sst-file-sizes-on-db-open o t)
    (rocksdb-options-get-skip-checking-sst-file-sizes-on-db-open o)
    (rocksdb-options-set-enable-blob-files o t)
    (rocksdb-options-get-enable-blob-files o)
    (rocksdb-options-set-enable-pipelined-write o t)
    (rocksdb-options-get-enable-pipelined-write o)
    (rocksdb-options-set-unordered-write o t)
    (rocksdb-options-get-unordered-write o)
    (rocksdb-options-set-allow-mmap-reads o t)
    (rocksdb-options-get-allow-mmap-reads o)
    (rocksdb-options-set-allow-mmap-writes o t)
    (rocksdb-options-get-allow-mmap-writes o)
    (rocksdb-options-set-use-direct-reads o t)
    (rocksdb-options-get-use-direct-reads o)
    (rocksdb-options-set-use-direct-io-for-flush-and-compaction o t)
    (rocksdb-options-get-use-direct-io-for-flush-and-compaction o)
    (rocksdb-options-set-is-fd-close-on-exec o t)
    (rocksdb-options-get-is-fd-close-on-exec o)
    (rocksdb-options-set-inplace-update-support o t)
    (rocksdb-options-get-inplace-update-support o)
    (rocksdb-options-set-advise-random-on-open o t)
    (rocksdb-options-get-advise-random-on-open o)
    (rocksdb-options-set-atomic-flush o t)
    (rocksdb-options-get-atomic-flush o)
    (rocksdb-options-set-manual-wal-flush o t)
    (rocksdb-options-get-manual-wal-flush o)
    (rocksdb-options-set-avoid-unnecessary-blocking-io o t)
    (rocksdb-options-get-avoid-unnecessary-blocking-io o)
    ;; this is full-width value 0-255, not boolean
    (rocksdb-options-set-level-compaction-dynamic-level-bytes o 20)
    (rocksdb-options-get-level-compaction-dynamic-level-bytes o)
    ;; int
    (rocksdb-options-set-compression-options-parallel-threads o 4)
    (rocksdb-options-get-compression-options-parallel-threads o)
    (rocksdb-options-set-info-log-level o 1)
    (rocksdb-options-get-info-log-level o)
    (rocksdb-options-set-max-open-files o 100)
    (rocksdb-options-get-max-open-files o)
    (rocksdb-options-set-max-file-opening-threads o 4)
    (rocksdb-options-get-max-file-opening-threads o)
    (rocksdb-options-set-compression-options-zstd-max-train-bytes o 1024)
    (rocksdb-options-get-compression-options-zstd-max-train-bytes o)
    (rocksdb-options-set-num-levels o 4)
    (rocksdb-options-get-num-levels o)
    (rocksdb-options-set-level0-file-num-compaction-trigger o 16)
    (rocksdb-options-get-level0-file-num-compaction-trigger o)
    (rocksdb-options-set-level0-slowdown-writes-trigger o 1024)
    (rocksdb-options-get-level0-slowdown-writes-trigger o)
    (rocksdb-options-set-level0-stop-writes-trigger o 1024)
    (rocksdb-options-get-level0-stop-writes-trigger o)
    (rocksdb-options-set-target-file-size-multiplier o 4)
    (rocksdb-options-get-target-file-size-multiplier o)
    ;; size-t
    (rocksdb-options-set-write-buffer-size o 1024)
    (rocksdb-options-get-write-buffer-size o)
    (rocksdb-options-set-db-write-buffer-size o 1024)
    (rocksdb-options-get-db-write-buffer-size o)
    ;; unsigned-long
    (rocksdb-options-set-compression-options-max-dict-buffer-bytes o 1024)
    (rocksdb-options-get-compression-options-max-dict-buffer-bytes o)
    (rocksdb-options-set-max-total-wal-size o 1024)
    (rocksdb-options-get-max-total-wal-size o)
    (rocksdb-options-set-target-file-size-base o 1024)
    (rocksdb-options-get-target-file-size-base o)
    (rocksdb-options-set-max-bytes-for-level-base o 1024)
    (rocksdb-options-get-max-bytes-for-level-base o)
    ;; double
    ;; (rocksdb-options-set-max-bytes-for-level-multiplier o (the double-float (/ 1 3)))
    )
  (let ((opts (rocksdb-options-create))
        (wopts (rocksdb-writeoptions-create))
        (ropts (rocksdb-readoptions-create))
        (bopts (rocksdb-block-based-options-create)))
    (rocksdb-options-set-create-if-missing opts t)
    ;; cleanup
    (rocksdb-options-destroy opts)
    (rocksdb-writeoptions-destroy wopts)
    (rocksdb-readoptions-destroy ropts)
    (rocksdb-block-based-options-destroy bopts)))

(defun make-errptr ()
  (make-alien rocksdb-errptr))

(deftest db-basic ()
  "Test basic RocksDB functionality. Inserts KV pair into a temporary
DB where K and V are both Lisp strings."
  (let* ((opts (test-opts))
         (path (rocksdb-test-dir))
         (db (rocksdb-open opts path nil))
         (key (genkey))
         (val (genval))
	 (klen (length key))
	 (vlen (length val))
         (wopts (rocksdb-writeoptions-create))
         (ropts (rocksdb-readoptions-create)))
    (with-alien ((k (* unsigned-char) (make-alien unsigned-char klen))
                 (v (* unsigned-char) (make-alien unsigned-char vlen))
                 (errptr rocksdb-errptr nil))
      ;; copy KEY to K
      (setfa k key)
      ;; copy VAL to V
      (setfa v val)
      ;; put K:V in DB
      (rocksdb-put db 
                   wopts
                   k
                   klen
                   v
                   vlen
                   errptr)
      (is (null-alien errptr))
      ;; get V from DB given K
      (rocksdb:rocksdb-cancel-all-background-work db t)
      (rocksdb-get db ropts k klen (make-alien size-t vlen) errptr)
      (is (null-alien errptr))
      ;; copy V to RVAL and validate
      (let ((rval (make-array vlen :element-type 'unsigned-byte)))
	(loop for i from 0 below vlen do (let ((x (deref v i))) (setf (aref rval i) x)))
	(is (string= (octets-to-string val) (concatenate 'string (map 'vector #'code-char rval)))))
      (rocksdb-delete db wopts k klen errptr)
      (is (null-alien errptr))
      (rocksdb-writeoptions-destroy wopts)
      (rocksdb-readoptions-destroy ropts)
      (rocksdb-cancel-all-background-work db nil)
      (rocksdb-close db)
      (rocksdb-destroy-db opts path errptr)
      (is (null-alien errptr))
      (rocksdb-options-destroy opts))))

(deftest sstfiles ()
  "Test SST file write/ingest functionality."
  (let* ((opts (test-opts))
         (path (rocksdb-test-dir))
         (file (rocksdb-test-file))
         (db (rocksdb-open opts path nil))
         (key (genkey))
         (val (genval))
         (klen (length key))
         (vlen (length val))
         (eopts (rocksdb-envoptions-create))
         (iopts (rocksdb-ingestexternalfileoptions-create))
         (ropts (rocksdb-readoptions-create))
         (writer (rocksdb-sstfilewriter-create eopts opts)))
    (with-alien ((k (* unsigned-char) (make-alien unsigned-char klen))
                 (v (* unsigned-char) (make-alien unsigned-char vlen))
                 (flist (array c-string 1))
                 (errptr rocksdb-errptr nil))
      ;; copy KEY to K
      (setfa k key)
      ;; copy VAL to V
      (setfa v val)
      (setf (deref flist 0) file)
      ;; create writer
      (rocksdb-sstfilewriter-open writer file errptr)
      ;; insert rows into sst file
      (rocksdb-sstfilewriter-put writer k klen v vlen errptr)
      (is (null-alien errptr))
      (rocksdb-sstfilewriter-finish writer errptr)
      (is (null-alien errptr))
      ;; ingest sst file
      (rocksdb-ingest-external-file db (cast flist (* c-string)) 1 iopts errptr)
      (is (null-alien errptr))
      (let ((vres (make-array vlen :element-type 'octet :fill-pointer 0)))
        (is (string= (octets-to-string val) (cast (rocksdb-get db ropts k klen (make-alien size-t vlen) errptr) c-string))))
      
      ;; rocksdb-sstfilewriter-file-size
      (rocksdb-sstfilewriter-destroy writer)
      (rocksdb-close db)
      (rocksdb-destroy-db opts path errptr)
      (rocksdb-options-destroy opts)
      (rocksdb-envoptions-destroy eopts)
      (delete-file file)
      (is (null-alien errptr)))))

(deftest stats ()
  "Test statistics and performance-context related functionality."
  (rocksdb-set-perf-level (rocksdb-perf-level "enable-time-except-for-mutex"))
  (let* ((opts (test-opts))
         (path (rocksdb-test-dir))
         (db (rocksdb-open opts path nil))
         (key (random-bytes 100))
         (val (random-bytes 100000))
         (klen (length key))
         (vlen (length val))
         (wopts (rocksdb-writeoptions-create))
         (ropts (rocksdb-readoptions-create))
         (ctx (rocksdb::rocksdb-perfcontext-create))
         (hist (rocksdb-statistics-histogram-data-create)))
    (with-alien ((k (* (unsigned 8)) (make-alien (unsigned 8) klen))
                 (v (* (unsigned 8)) (make-alien (unsigned 8) vlen))
                 (errptr rocksdb-errptr nil))
      ;; copy KEY to K
      (setfa k key)
      ;; copy VAL to V
      (setfa v val)
      ;; put K:V in DB
      (rocksdb-put db 
                   wopts
                   k
                   klen
                   v
                   vlen
                   errptr)
      
      (debug! "stats:" (rocksdb-options-statistics-get-string opts))
      (rocksdb-options-statistics-get-histogram-data opts 5 hist) ;; histogram data types? uint64 somewhere
      (debug! "count:" (rocksdb-statistics-histogram-data-get-count hist))
      (rocksdb-perfcontext-reset ctx)
      ;; ...
      (rocksdb-set-perf-level (rocksdb-perf-level "disable"))
      (rocksdb-statistics-histogram-data-destroy hist)
      (rocksdb-close db)
      (rocksdb-destroy-db opts path errptr)
      (rocksdb-options-destroy opts))))

;; stats-dump-period-sec

(deftest blob ()
  "Test BlobDB functionality."
  (let* ((opts (test-opts))
         (path (rocksdb-test-dir))
         db
         (key (random-bytes 8))
         (val (make-array 9999 :initial-element 36))
         (klen (length key))
         (vlen (length val))
         (wopts (rocksdb-writeoptions-create))
         (ropts (rocksdb-readoptions-create))
         (bcache (rocksdb-cache-create-lru 128)))
    (rocksdb-options-set-enable-blob-files opts t)
    (rocksdb-options-set-enable-blob-gc opts t)
    (rocksdb-options-set-blob-compression-type opts (rocksdb-compression-backend "zstd"))
    (rocksdb-options-set-blob-cache opts bcache)
    (setf db (rocksdb-open opts path nil))

    (with-alien ((k (* (unsigned 8)) (make-alien (unsigned 8) klen))
                 (v (* (unsigned 8)) (make-alien (unsigned 8) vlen))
                 (errptr rocksdb-errptr nil))
      (debug! "min blob file size: " (rocksdb-options-get-min-blob-size opts))
      (debug! "max blob file size: " (rocksdb-options-get-blob-file-size opts))

      ;; copy KEY to K
      (setfa k key)
      ;; copy VAL to V
      (setfa v val)
      ;; put K:V in DB - 
      (rocksdb-put db 
                   wopts
                   k
                   klen
                   v
                   vlen
                   errptr)
      (is (null-alien errptr))
      (rocksdb:rocksdb-flush db (rocksdb-flushoptions-create) errptr)
      (is (null-alien errptr))
      (is (stringp
           (cast
            (rocksdb-get db
                         ropts
                         k
                         klen
                         (make-alien size-t vlen)
                         errptr)
            c-string)))
      (rocksdb-writeoptions-destroy wopts)
      (rocksdb-readoptions-destroy ropts)
      (rocksdb-close db)
      (rocksdb-destroy-db opts path errptr)
      (is (null-alien errptr))
      (rocksdb-options-destroy opts))))

(deftest transaction ()
  "Test simple transactions using both TransactionDB and OptimisticTransactionDB."
  (let* ((opts (test-opts))
         (path (rocksdb-test-dir))
         (db (rocksdb-open opts path nil))
         (key (genkey))
         (val (genval))
         (klen (length key))
         (vlen (length val))
         (wopts (rocksdb-writeoptions-create))
         (ropts (rocksdb-readoptions-create)))
    (with-alien ((k (* (unsigned 8)) (make-alien (unsigned 8) klen))
                 (v (* (unsigned 8)) (make-alien (unsigned 8) vlen))
                 (errptr rocksdb-errptr nil))
      ;; copy KEY to K
      (setfa k key)
      ;; copy VAL to V
      (setfa v val)
      ;; put K:V in DB - 
      (rocksdb-writeoptions-destroy wopts)
      (rocksdb-readoptions-destroy ropts)
      (rocksdb-close db)
      (rocksdb-destroy-db opts path errptr)
      (rocksdb-options-destroy opts)
      (is (null-alien errptr)))))

(deftest metadata ()
  "Test metadata functionality :: cf-meta -> level-meta -> sst-file-meta"
  nil)

(deftest properties ()
  "Test the ROCKSDB-GET-PROPERTY-* functions."
  ;; *rocksdb-properties*
  (let* ((opts (test-opts))
         (path (rocksdb-test-dir))
         (db (rocksdb-open opts path nil))
         (key (genkey))
         (val (genval))
         (klen (length key))
         (vlen (length val))
         (wopts (rocksdb-writeoptions-create))
         (ropts (rocksdb-readoptions-create)))
    (is (stringp (debug! (rocksdb-property-value db (make-alien-string "rocksdb.stats")))))
    (is (zerop (parse-integer (rocksdb-property-value db (make-alien-string "rocksdb.num-files-at-level3")))))))

(deftest merge ()
  "Test low-level merge-operator functionality using Alien Callbacks."
  (is (with-alien ((k (array unsigned-char))
                   (v (array unsigned-char))
                   (ops (array (array unsigned-char)))
                   (s (array unsigned-char)))
        (alien-funcall
         (alien-callable-function
          'rocksdb-concat-full-merge)
         k 0 v 0 ops (make-alien size-t 0) 0 s (make-alien size-t 0))))
  (is
   (not
    (with-alien ((k (array unsigned-char))
                 (ops (array (array unsigned-char)))
                 (s (array unsigned-char)))
      (alien-funcall
       (alien-callable-function
        'rocksdb-concat-partial-merge)
       k 0 ops (make-alien size-t 0) 0 s (make-alien size-t 0)))))
  (alien-callable-function 'rocksdb-concat-full-merge)
  (alien-callable-function 'rocksdb-concat-partial-merge)
  (is (integerp
       (parse-integer
        (string-trim "rocksdb:" (alien-funcall (alien-callable-function 'rocksdb-name))))))
  ;; returns No Value
  (with-alien ((str c-string (make-alien-string ""))
               (state (* t)))
    (is (null (alien-funcall (alien-callable-function 'rocksdb-delete-value) state str 1))))

  (is (null (alien-funcall (alien-callable-function 'rocksdb-destructor) (make-alien (* t)))))

  ;; null merge op
  (with-alien ((state (* t))
               (destructor (* rocksdb-destructor-function))
               (full-merge (* rocksdb-full-merge-function))
               (partial-merge (* rocksdb-partial-merge-function))
               (delete-value (* rocksdb-delete-value-function))
               (name (* rocksdb-name-function)))
    (is (typep (rocksdb-mergeoperator-create state destructor full-merge partial-merge delete-value name)
               '(alien (* rocksdb-mergeoperator)))))

  ;; concat merge op
  (with-alien ((state (* t))
               (destructor (* rocksdb-destructor-function) (alien-sap (alien-callable-function 'rocksdb-destructor)))
               (full-merge (* rocksdb-full-merge-function) (alien-sap (alien-callable-function 'rocksdb-concat-full-merge)))
               (partial-merge (* rocksdb-partial-merge-function) (alien-sap (alien-callable-function 'rocksdb-concat-partial-merge)))
               (delete-value (* rocksdb-delete-value-function) (alien-sap (alien-callable-function 'rocksdb-concat-delete-value)))
               (name (* rocksdb-name-function) (alien-sap (alien-callable-function 'rocksdb-concat-merge-name))))
    (is (typep (rocksdb-mergeoperator-create state destructor full-merge partial-merge delete-value name)
               '(alien (* rocksdb-mergeoperator))))))

(deftest comparator ()
  "Test low-level comparator API."
  (with-alien ((state (* t))
               (destructor (* rocksdb-destructor-function) (alien-sap (alien-callable-function 'rocksdb-destructor)))
               (compare (* rocksdb-compare-function) (alien-sap (alien-callable-function 'rocksdb-compare-never)))
               (compare-with-ts (* rocksdb-compare-with-ts-function))
               (compare-without-ts (* rocksdb-compare-without-ts-function))
               (name (* rocksdb-name-function) (alien-sap (alien-callable-function 'rocksdb-name))))
    (is (typep (rocksdb-comparator-create state destructor compare name)
               '(alien (* rocksdb-comparator))))
    (is (typep (rocksdb-comparator-with-ts-create state destructor compare compare-with-ts compare-without-ts name)
               '(alien (* rocksdb-comparator))))))

(deftest compaction ()
  "Test low-level compactionfilter API."
  (with-alien ((state (* t))
               (context (* rocksdb-compactionfiltercontext)))
    (is (typep
         (rocksdb-compactionfilter-create state
                                          (alien-sap (alien-callable-function 'rocksdb-destructor))
(alien-sap (alien-callable-function 'rocksdb-filter-never))
                                          (alien-sap (alien-callable-function 'rocksdb-name)))
         '(alien (* rocksdb-compactionfilter))))
    (is (typep
         (rocksdb-compactionfilterfactory-create state
                                                 (alien-sap (alien-callable-function 'rocksdb-destructor))
                                                 (alien-sap (alien-callable-function
                                                             'rocksdb-create-compaction-filter-never))
                                                 (alien-sap (alien-callable-function 'rocksdb-name)))
         '(alien (* rocksdb-compactionfilterfactory))))))
    
(deftest logger ()
  "Test logging functionality.")
