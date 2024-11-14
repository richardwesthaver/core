;;; rdb.lisp --- RDB Structure

;; High-level RocksDB API

;;; Commentary:

;; This package produces reasonably safe wrappers for RocksDB functionality
;; with alien structures wrapped in STRUCT types, conditions,
;; etc. Additionally this package provides the RDB-DATABASE and RDB-STORE
;; classes which implement the OBJ/DB and OBJ/STORE protocols respectively.

;;; Code:
(in-package :rdb)
(pushnew :rdb *features*)
