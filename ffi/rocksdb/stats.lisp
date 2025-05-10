;;; rocksdb/stats.lisp --- Database Statistics

;;

;;; Code:
(in-package :rocksdb)
(defar rocksdb-statistics-histogram-data-create (* rocksdb-statistics-histogram-data))
(defar rocksdb-statistics-histogram-data-destroy void
  (data (* rocksdb-statistics-histogram-data)))
(defar rocksdb-statistics-histogram-data-get-median double
  (data (* rocksdb-statistics-histogram-data)))
(defar rocksdb-statistics-histogram-data-get-p95 double
  (data (* rocksdb-statistics-histogram-data)))
(defar rocksdb-statistics-histogram-data-get-p99 double
  (data (* rocksdb-statistics-histogram-data)))
(defar rocksdb-statistics-histogram-data-get-average double
  (data (* rocksdb-statistics-histogram-data)))
(defar rocksdb-statistics-histogram-data-get-std-dev double
  (data (* rocksdb-statistics-histogram-data)))
(defar rocksdb-statistics-histogram-data-get-max double
  (data (* rocksdb-statistics-histogram-data)))
(defar rocksdb-statistics-histogram-data-get-count (unsigned 64)
  (data (* rocksdb-statistics-histogram-data)))
(defar rocksdb-statistics-histogram-data-get-sum (unsigned 64)
  (data (* rocksdb-statistics-histogram-data)))
(defar rocksdb-statistics-histogram-data-get-min double
  (data (* rocksdb-statistics-histogram-data)))



