;;; rdb.lisp --- RocksDB client

;;; Code:
(defpackage :bin/rdb
  (:use :cl :rdb :std :cli/clap :log :clap :db :seq))

(in-package :bin/rdb)
(rocksdb:load-rocksdb t)
(defvar *rdb*)
(defopt rdb-target-db (or *arg* "rdb"))

;; (defopt rdb-config (init-rdb-user-config (parse-file-opt *arg*)))

(defcmd rdb-new ()
  (set-db-opt *rdb* :error-if-exists t)
  (open-db *rdb*)
  (println (rdb-name *rdb*)))

(defcmd rdb-show ()
  (let* ((db-path (cli-opt-val (car (find-opts "db" *cli*))))
         (*rdb* (create-rdb db-path :open nil)))
    (if (and (null db-path) (zerop *argc*))
        (mapc (lambda (x) (println (format nil "~a ~a" (car x) (cdr x))))
              (hash-table-alist (backfill-opts (default-rdb-opts) :full t)))
        (with-rdb (db (create-rdb db-path :open t))
          (println (hash-table-alist (backfill-opts db)))
          (with-iter (it (iter db))
            (seek-to-first it)
            (loop while (iter-valid-p it)
                  do (progn
                       (format t "~A : ~A~%"
                               (sb-ext:octets-to-string (key it) :external-format '(:ascii :replacement #\_))
                               (val it))
                       (next it))))))))

(defcmd rdb-set ()
  (if (> 2 *argc*)
      (rdb-error "missing args: KEY VAL")
      (with-rdb (db *rdb*)
        (open-db db)
        (insert-key  db (pop *args*) (pop *args*)))))

(defcmd rdb-get ()
  (if (> 1 *argc*)
      (rdb-error "missing arg: KEY")
      (with-rdb (db *rdb*)
        (open-db db)
        (when-let ((val (get-key db (car *args*))))
          (println val)))))

(defcmd rdb-destroy ()
  (destroy-db *rdb*))

(defcmd rdb-fuzz ()
  (with-rdb (db *rdb*)
    (open-db db)
    (let ((val (make-array 32 :element-type 'octet)))
      (dotimes (i (if (zerop *argc*) 1000 (parse-integer (car *args*))))
        (nreversef val)
        (let ((seed (random 32)))
          (dotimes (ii seed)
            (setf (aref val ii) (random 256))))
        (nreversef val)
        (put-key db
                 (sb-ext:string-to-octets (string (gensym "foo")))
                 val)))))

(define-cli *rdb-cli*
  :help t
  :name "rdb"
  :version "0.1.0"
  :thunk rdb-show
  :description "A simple helper for RocksDB."
  :opts ((:name "level" :description "set log level" :thunk level-opt)
         (:name "version" :description "print version" :thunk version-opt)
         (:name "db" :description "target db" :thunk rdb-target-db :kind dir))
  :cmds ((:name new
          :thunk rdb-new)
         (:name show
          :thunk rdb-show)
         (:name set :thunk rdb-set)
         (:name get :thunk rdb-get)
         (:name fuzz :thunk rdb-fuzz)
         (:name destroy :thunk rdb-destroy)))

(defmain start-rdb ()
  (let ((*log-level* :info))
    (with-cli (*rdb-cli* :args (cli:args))
      (when (find-opt "help" *cli*)
        (print-help *cli*)
        (sb-ext:exit :code 0))
      (with-db (db :db (create-rdb (do-opt (car (find-opts "db" *cli*)))) :open t :close t)
        (do-cmd *cli*)))))
