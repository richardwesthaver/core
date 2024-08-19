;;; macs.lisp --- RocksDB Alien Macros

;; Convenience Macros for working with RocksDB Alien types

;;; Code:
(in-package :rocksdb)


(defmacro define-merge-operator-callbacks (name full partial state destructor delete-fn))
