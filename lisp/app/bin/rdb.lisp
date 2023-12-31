;;; rdb.lisp --- RocksDB client

;;; Code:
(uiop:define-package :bin/rdb
  (:use :cl :rdb :std :cli)
  (:export :main))

(in-package :bin/rdb)

(defopt rdb-help (print-help $cli))
(defopt rdb-version (print-version $cli))
(defopt rdb-log-level (setq *log-level* (if $val :debug nil)))
;; (defopt rdb-config (init-rdb-user-config (parse-file-opt $val)))
(define-cli $cli
  :name "rdb"
  :version "0.1.0"
  :description "richard's database"
  :opts (make-opts
	  (:name level :global t :description "set the log level" :thunk rdb-log-level)
	  (:name help :global t :description "print help" :thunk rdb-help)
	  (:name version :global t :description "print version" :thunk rdb-version)))

(defun run ()
  (with-cli (opts cmds args) $cli
    (do-cmd $cli)
    (debug-opts $cli)))

(defmain ()
  (rocksdb:load-rocksdb t)
  (run))
