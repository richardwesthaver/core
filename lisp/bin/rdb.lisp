;;; rdb.lisp --- RocksDB client

;;; Code:
(uiop:define-package :bin/rdb
    (:use :cl :rdb :std :cli :log)
  (:export :main))

(in-package :bin/rdb)
(rocksdb:load-rocksdb t)
(defopt rdb-help (print-help $cli))
(defopt rdb-version (print-version $cli))
(defopt rdb-log-level (when $val (setq *log-level* :debug)))
(defvar *rdb*)
(defopt rdb-target-db (setq *rdb* (create-db (or $val "rdb") :open nil)))

;; (defopt rdb-config (init-rdb-user-config (parse-file-opt $val)))

(defcmd help (print-help $cli))

(defcmd new
  (set-opt *rdb* :error-if-exists t)
  (open-db *rdb*)
  (println (rdb-name *rdb*)))

(defcmd show
  (let ((db-path (find-opt $cli "db" t)))
    (if (and (null db-path)
             (or (zerop $argc) (equal (car $args) "opts")))
        (mapc (lambda (x) (println (format nil "~a ~a" (car x) (cdr x))))
              (hash-table-alist (backfill-opts (default-rdb-opts) :full t)))
        (with-db (db (create-db (cli/clap::cli-opt-val db-path) :open t))
          (println (hash-table-alist (backfill-opts db)))))))

(defcmd insert
  (if (> 2 $argc)
      (rdb-error "missing args: KEY VAL")
      (with-db (db *rdb*)
        (open-db db)
        (insert-key  db (pop $args) (pop $args)))))

(define-cli $cli
  :name "rdb"
  :version "0.1.0"
  :thunk help
  :description "richard's database"
  :opts (make-opts
          (:name "level" :global t :description "set the log level" :thunk rdb-log-level)
          (:name "help" :global t :description "print help" :thunk rdb-help)
          (:name "version" :global t :description "print version" :thunk rdb-version)
          (:name "db" :global t :description "target db" :thunk rdb-target-db :kind dir))
  :cmds (make-cmds
          (:name new :thunk new)
          (:name show :thunk show)
          (:name insert :thunk insert)))

(defmain ()
  (let ((*log-level* :info))
    (with-cli (opts cmds args) $cli
      ;; FIXME 2024-05-07: 
      (do-opt (find-opt $cli "db"))
      (prog1 (do-cmd $cli)
        (close-db *rdb*)))))
