;;; rocksdb/tests.lisp --- RocksDB tests

;;; Code:
(defpackage :rocksdb/tests
  (:use :cl :std :rt :rocksdb :sb-ext :sb-alien))

(in-package :rocksdb/tests)

(defsuite :rocksdb)
(in-suite :rocksdb)

(load-rocksdb)

(defun rocksdb-test-dir ()
  (format nil "/tmp/~A/" (gensym "rocksdb-tests-")))

(defun test-opts () 
  (let ((default (rocksdb-options-create)))
    (rocksdb-options-set-create-if-missing default 1)
    default))

;; not thread safe (gensym-counter)
(defun genkey (&optional prefix) (string-to-octets (symbol-name (gensym (or prefix "key")))))
(defun genval (&optional prefix) (string-to-octets (symbol-name (gensym (or prefix "val")))))

(defun random-array (dim &optional (limit 4096))
  (let ((r (make-array dim)))
    (dotimes (i (array-total-size r) r)
      (setf (row-major-aref r i) (random limit)))))

(defun random-bytes (dim)
  (random-array dim 256))

(defmacro with-opt ((var create destroy) &body body)
  `(let ((,var ,create))
     (unwind-protect (progn ,@body)
       ,destroy)))

(deftest opts ()
  (with-opt (o (rocksdb-options-create) (rocksdb-options-destroy o))
    ;; unsigned-char
    (rocksdb-options-set-create-if-missing o 1)
    (rocksdb-options-get-create-if-missing o)
    (rocksdb-options-set-create-missing-column-families o 1)
    (rocksdb-options-get-create-missing-column-families o)
    (rocksdb-options-set-error-if-exists o 1)
    (rocksdb-options-get-error-if-exists o)
    (rocksdb-options-set-paranoid-checks o 1)
    (rocksdb-options-get-paranoid-checks o)
    (rocksdb-options-set-compression-options-use-zstd-dict-trainer o 1)
    (rocksdb-options-get-compression-options-use-zstd-dict-trainer o)
    (rocksdb-options-set-enable-blob-gc o 1)
    (rocksdb-options-get-enable-blob-gc o)
    (rocksdb-options-set-allow-ingest-behind o 1)
    (rocksdb-options-get-allow-ingest-behind o)
    (rocksdb-options-set-skip-stats-update-on-db-open o 1)
    (rocksdb-options-get-skip-stats-update-on-db-open o)
    (rocksdb-options-set-skip-checking-sst-file-sizes-on-db-open o 1)
    (rocksdb-options-get-skip-checking-sst-file-sizes-on-db-open o)
    (rocksdb-options-set-enable-blob-files o 1)
    (rocksdb-options-get-enable-blob-files o)
    (rocksdb-options-set-enable-pipelined-write o 1)
    (rocksdb-options-get-enable-pipelined-write o)
    (rocksdb-options-set-unordered-write o 1)
    (rocksdb-options-get-unordered-write o)
    (rocksdb-options-set-allow-mmap-reads o 1)
    (rocksdb-options-get-allow-mmap-reads o)
    (rocksdb-options-set-allow-mmap-writes o 1)
    (rocksdb-options-get-allow-mmap-writes o)
    (rocksdb-options-set-use-direct-reads o 1)
    (rocksdb-options-get-use-direct-reads o)
    (rocksdb-options-set-use-direct-io-for-flush-and-compaction o 1)
    (rocksdb-options-get-use-direct-io-for-flush-and-compaction o)
    (rocksdb-options-set-is-fd-close-on-exec o 1)
    (rocksdb-options-get-is-fd-close-on-exec o)
    (rocksdb-options-set-inplace-update-support o 1)
    (rocksdb-options-get-inplace-update-support o)
    (rocksdb-options-set-advise-random-on-open o 1)
    (rocksdb-options-get-advise-random-on-open o)
    (rocksdb-options-set-atomic-flush o 1)
    (rocksdb-options-get-atomic-flush o)
    (rocksdb-options-set-manual-wal-flush o 1)
    (rocksdb-options-get-manual-wal-flush o)
    (rocksdb-options-set-avoid-unnecessary-blocking-io o 1)
    (rocksdb-options-get-avoid-unnecessary-blocking-io o)
    ;; this is full-width value 0-255, not boolean
    (rocksdb-options-set-level-compaction-dynamic-level-bytes o 1)
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
    (rocksdb-options-set-create-if-missing opts 1)
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
    (with-alien ((k (* char) (make-alien char klen))
                 (v (* char) (make-alien char vlen))
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
