;;; rdb.lisp --- RDB Structure

;; High-level RocksDB API

;;; Commentary:

;; This package produces reasonably safe wrappers for RocksDB functionality
;; with alien structures wrapped in STRUCT types, conditions,
;; etc. 

;; Additionally this package implements the following protocols:

;; RDB-DATABASE (OBJ/DB)
;; RDB-STORE (OBJ/STORE)
;; RDB-SCHEMA (OBJ/SCHEMA)
;; RDB-QUERY (OBJ/QUERY)
;; RDB-LOGGER (OBJ/LOG)

;;; Code:
(in-package :rdb)
(pushnew :rdb *features*)
