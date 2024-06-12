;;; rdb.lisp --- RocksDB client

;;; Code:
(uiop:define-package :bin/rdb
    (:use :cl :rdb :std :cli/clap :log)
  (:export :main))

(in-package :bin/rdb)
(rocksdb:load-rocksdb t)
(defopt rdb-help (print-help $cli))
(defopt rdb-version (print-version $cli))
(defopt rdb-log-level (when $val (setq *log-level* :debug)))
(defvar *rdb*)
(defopt rdb-target-db (setq *rdb* (create-db (or $val "rdb") :open nil)))

;; (defopt rdb-config (init-rdb-user-config (parse-file-opt $val)))

(defcmd rdb-new
  (set-opt *rdb* :error-if-exists t)
  (open-db *rdb*)
  (println (rdb-name *rdb*)))

(defcmd rdb-show
  (let ((db-path (cli-opt-val (car (find-opts $cli "db")))))
    (if (and (null db-path) (zerop $argc))
        (mapc (lambda (x) (println (format nil "~a ~a" (car x) (cdr x))))
              (hash-table-alist (backfill-opts (default-rdb-opts) :full t)))
        (with-db (db (create-db db-path :open t))
          (println (hash-table-alist (backfill-opts db)))
          (with-iter (it (create-iter db))
            (iter-seek-to-first it)
            (loop while (iter-valid-p it)
                  do (progn
                       (format t "~A : ~A~%"
                               (sb-ext:octets-to-string (iter-key it) :external-format '(:ascii :replacement #\_))
                               (iter-val it))
                       (iter-next it))
                  finally (rocksdb::rocksdb-iter-destroy %it)))))))

(defcmd rdb-set
  (if (> 2 $argc)
      (rdb-error "missing args: KEY VAL")
      (with-db (db *rdb*)
        (open-db db)
        (insert-key  db (pop $args) (pop $args)))))

(defcmd rdb-get
  (if (> 1 $argc)
      (rdb-error "missing arg: KEY")
      (with-db (db *rdb*)
        (open-db db)
        (when-let ((val (get-key db (car $args))))
          (println val)))))

(defcmd rdb-destroy
  (destroy-db *rdb*))

(defcmd rdb-fuzz
  (with-db (db *rdb*)
    (open-db db)
    (let ((val (make-array 32 :element-type 'octet)))
      (dotimes (i (if (zerop $argc) 1000 (parse-integer (car $args))))
        (nreversef val)
        (let ((seed (random 32)))
          (dotimes (ii seed)
            (setf (aref val ii) (random 256))))
          (nreversef val)
          (put-key db
                   (sb-ext:string-to-octets (string (gensym "foo")))
                   val)))))

(define-cli $cli
  :name "rdb"
  :version "0.1.0"
  :thunk rdb-show
  :description "A simple helper for RocksDB."
  :opts (make-opts
          (:name "level" :global t :description "set the log level" :thunk rdb-log-level)
          (:name "help" :global t :description "print help" :thunk rdb-help)
          (:name "version" :global t :description "print version" :thunk rdb-version)
          (:name "db" :global t :description "target db" :thunk rdb-target-db :kind dir))
  :cmds (make-cmds
          (:name new :thunk rdb-new)
          (:name show :thunk rdb-show)
          (:name set :thunk rdb-set)
          (:name get :thunk rdb-get)
          (:name fuzz :thunk rdb-fuzz)
          (:name destroy :thunk rdb-destroy)))

(defmain ()
  (let ((*log-level* :info))
    (with-cli (opts cmds args) $cli
      ;; FIXME 2024-05-07: needs to be triggered explicitly - need to support
      ;; running global opt thunks even when no arg present - macro key
      (if (active-cmds $cli)
          (prog2 (do-opt (car (find-opts $cli "db")))
              (do-cmd $cli)
            (close-db *rdb*))
          (print-help $cli)))))
