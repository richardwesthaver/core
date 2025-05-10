;;; logger.lisp --- RocksDB Logger

;; RocksDB Logger Lisp FFI

;;; Commentary:

;; ref: https://github.com/facebook/rocksdb/wiki/Logger

;;; Code:
(in-package :rocksdb)

(defar rocksdb-logger-destroy void
  (logger (* rocksdb-logger)))

(define-alien-type rocksdb-log-function
  (function void
            (* t)
            unsigned
            c-string
            size-t))

(defar rocksdb-logger-create-stderr-logger (* rocksdb-logger)
  (log-level int)
  (prefix c-string))

(defar rocksdb-logger-create-callback-logger (* rocksdb-logger)
  (log-level int)
  (fn (* rocksdb-log-function))
  (priv (* t)))

;; logger callback
(define-alien-callable rocksdb-log-default void
    ((priv (* t))
     (lev unsigned)
     (msg c-string)
     (len size-t))
  (declare (ignore priv len lev))
  (log:log-message :info nil msg))
