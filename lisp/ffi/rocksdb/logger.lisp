;;; logger.lisp --- RocksDB Logger

;; RocksDB Logger Lisp FFI

;;; Code:
(in-package :rocksdb)

(define-alien-routine rocksdb-logger-destroy void
  (logger (* rocksdb-logger)))

(define-alien-type rocksdb-log-function
  (function void
            (* t)
            unsigned
            c-string
            size-t))

(define-alien-routine rocksdb-logger-create-stderr-logger (* rocksdb-logger)
  (log-level int)
  (prefix c-string))

(define-alien-routine rocksdb-logger-create-callback-logger (* rocksdb-logger)
  (log-level int)
  (fn (* rocksdb-log-function))
  (priv (* t)))

;; logger callback
(define-alien-callable rocksdb-log-default void
    ((priv (* t))
     (lev unsigned)
     (msg c-string)
     (len size-t))
  (log:debug! priv lev msg len))
