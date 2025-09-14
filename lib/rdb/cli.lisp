;;; cli.lisp --- RDB CLI

;; 

;;; Code:
(in-package :rdb/cli)
(rocksdb:load-rocksdb t)

(defopt rdb-config-opt (init-rdbrc (cli/clap/obj::parse-file-opt *arg*)))

(defopt rdb-path-opt (or *arg* "/tmp/rdb"))

(defcmd rdb-new ()
  (set-db-opt *db* :error-if-exists t)
  (open-db *db*)
  (println (name *db*)))

(defcmd rdb-show ()
  (let* ((db-path (cli-opt-val (car (find-opts "db" *cli*))))
	 (*db* (create-rdb db-path :open nil)))
    (if (and (null db-path) (zerop *argc*))
	(mapc (lambda (x) (println (format nil "~a ~a" (car x) (cdr x))))
	      (hash-table-alist (backfill-opts (default-rdb-opts) :full t)))
	(with-rdb (db (create-rdb db-path :open t))
	  (println (hash-table-alist (backfill-opts db)))
	  (with-iter (it (iter db))
	    seek-to-first
	    (loop while (iter-valid-p it)
		  do (progn
                       (format t "~A : ~A~%"
			       (sb-ext:octets-to-string key :external-format '(:ascii :replacement #\_))
			       val)
		       next)))))))

(defcmd rdb-set ()
  (if (> 2 *argc*)
      (rdb-error "missing args: KEY VAL")
      (with-rdb (db *db*)
	(open-db db)
	(insert-key  db (pop *args*) (pop *args*)))))

(defcmd rdb-get ()
  (if (> 1 *argc*)
      (rdb-error "missing arg: KEY")
      (with-rdb (db *db*)
	(open-db db)
	(when-let ((val (get-key db (car *args*))))
	  (println val)))))

(defcmd rdb-destroy ()
  (destroy-db *db*))

(defcmd rdb-fuzz ()
  (with-rdb (db *db*)
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
  :name "rdb"
  :package :rdb
  :help t
  :version "0.1.0"
  :description "Richard's Database"
  :thunk rdb-show
  :opts ((:name "level" :description "set log level" :thunk level-opt)
         (:name "version" :description "print version" :thunk version-opt)
         (:name "path" :description "database path" :thunk rdb-path-opt :type dir)
         (:name "config" :description "database configuration" :thunk rdb-config-opt :type file))
  :cmds ((:name new :thunk rdb-new)))
