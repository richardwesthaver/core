;;; rdb.lisp --- RocksDB client

;;; Code:
(uiop:define-package :bin/rdb
    (:use :cl :rdb :std :cli :log)
  (:export :main))

(in-package :bin/rdb)
(rocksdb:load-rocksdb t)
(defopt rdb-help (print-help $cli))
(defopt rdb-version (print-version $cli))
(defopt rdb-log-level (setq log:*log-level* (when $val (setq *log-level* :debug))))
;; (defopt rdb-config (init-rdb-user-config (parse-file-opt $val)))

(defcmd help (print-help $cli))
(defparameter *default-rdb-name* "rdb")

(defcmd new
  (let ((name (when $args (pop $args))))
    (with-db (db (create-db (or name *default-rdb-name*) :open t))
      (close-db db))))

(defcmd show
  (if (or (zerop $argc) (equal (car $args) "opts"))
      (mapc (lambda (x) (println (format nil "~a ~a" (car x) (cdr x)))) (hash-table-alist (backfill-opts (default-rdb-opts) :full t)))))

(define-cli $cli
  :name "rdb"
  :version "0.1.0"
  :thunk help
  :description "richard's database"
  :opts (make-opts
          (:name level :global t :description "set the log level" :thunk rdb-log-level)
          (:name help :global t :description "print help" :thunk rdb-help)
          (:name version :global t :description "print version" :thunk rdb-version))
  :cmds (make-cmds
          (:name new :thunk new)
          (:name show :thunk show)))

(defmain ()
  (with-cli (opts cmds args) $cli
    (do-cmd $cli)))
