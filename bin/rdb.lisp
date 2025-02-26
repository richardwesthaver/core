;;; rdb.lisp --- RocksDB client

;;; Code:
(defpackage :bin/rdb 
  (:use :cl :rdb :std :cli/clap :log :clap :db :seq))
(in-package :bin/rdb)
(load-package-cli :rdb)
(defmain start-rdb ()
  (let ((*log-level* :info))
    (with-cli (*rdb-cli* :args (cli:args))
      (when (find-opt "help" *cli*)
        (print-help *cli*)
        (sb-ext:exit :code 0))
      (with-db (db :db (create-rdb (do-opt (car (find-opts "db" *cli*)))) :open t :close t)
        (do-cmd *cli*)))))
