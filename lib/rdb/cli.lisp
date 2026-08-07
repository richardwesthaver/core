;;; cli.lisp --- RDB CLI

;; 

;;; Code:
(in-package :rdb/cli)
(init :commands :name :rdb :copy :cli :clean t)

(defcommand (:rdb new) ()
  (setf (opt *db* :error-if-exists) t)
  (open-db *db*)
  (println (name *db*)))

(defcommand (:rdb show) (db-path)
  (let ((*db* (rdb db-path :open nil)))
    (if (null db-path)
	(mapc (lambda (x) (println (format nil "~a ~a" (car x) (cdr x))))
	      (hash-table-alist (backfill-opts (default-rocksdb-options) :full t)))
	(with-rdb (db (rdb db-path :open t))
	  (println (hash-table-alist (backfill-opts db)))
	  (with-iter (it (iter db))
	    seek-to-first
	    (loop while iter-valid-p
		      do (progn
                   (format t "~A : ~A~%"
			               (sb-ext:octets-to-string key :external-format '(:ascii :replacement #\_))
			               val)
		           next)))))))

(defcommand (:rdb set) (k v)
  (with-rdb (db *db*)
    (open-db db)
    (insert-key db k v)))

(defcommand (:rdb get) (k)
  (with-rdb (db *db*)
    (open-db db)
    (when-let ((val (get-key db k)))
      (println val))))

(defcommand (:rdb destroy) ()
  (destroy-db *db*))

(defcommand (:rdb fuzz) (&optional val)
  (with-rdb (db *db*)
    (open-db db)
    (let ((%val (make-array 32 :element-type 'octet)))
      (dotimes (i (or val 1000))
	(nreversef %val)
	(let ((seed (random 32)))
	  (dotimes (ii seed)
	    (setf (aref val ii) (random 256))))
	(nreversef val)
	(put-key db
		 (sb-ext:string-to-octets (string (gensym "foo")))
		 val)))))

(save :commands :rdb)
#+todo
(define-cli "rdb" :version 0)
