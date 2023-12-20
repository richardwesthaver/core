(in-package :rdb)

(defun make-rocksdb-options (&optional init-fn)
  "Make and return RDB-OPTS. INIT-FN is an optional argument which must be a
lambda which takes a single parameter (the RDB-OPTS sap). It is used
to initialize the instance with custom configuration."
  (let ((opts (rocksdb-options-create)))
    (when init-fn (funcall init-fn opts))
    opts))

(defun default-rocksdb-options ()
  (make-rocksdb-options
   (lambda (o) (rocksdb-options-set-create-if-missing o 1))))

(defun open-db-raw (db-path &optional (opts (default-rocksdb-options)))
  (with-errptr (err 'open-db-error (list :db db-path))
    (let* ((db-path (if (pathnamep db-path)
                        (namestring db-path)
                        db-path)))
      (rocksdb-open opts db-path err))))

(defun close-db-raw (db)
  (rocksdb-close db))

(defun destroy-db-raw (path &optional (opt (rocksdb-options-create)))
  (with-errptr (err 'destroy-db-error (list :db path))
    (rocksdb-destroy-db opt path err)
    (rocksdb-options-destroy opt)))

;; (with-open-db-raw (db "/tmp/tmp-db") (print db))
;; (destroy-db-raw "/tmp/with-db-raw")

(defmacro with-open-db-raw ((db-var db-path &optional (opt (default-rocksdb-options))) &body body)
  `(let ((,db-var (open-db-raw ,db-path ,opt)))
     (unwind-protect (progn ,@body)
       (rocksdb-close ,db-var)
       (with-errptr (err 'rocksdb-error)
         ;; (rocksdb-destroy-db ,opt ,db-path err) ;; when :destroy only
         (rocksdb-options-destroy ,opt)))))

(defun put-kv-raw (db key val &optional (opts (rocksdb-writeoptions-create)))
  (let ((klen (length key))
	(vlen (length val)))
    (with-alien ((k (* char) (make-alien char klen))
		 (v (* char) (make-alien char vlen)))
      (setfa k key)
      (setfa v val)
      (with-errptr (err 'put-kv-error (list :db db :kv (cons key val)))
        (rocksdb-put db
		     opts
		     k
		     klen
		     v
		     vlen
		     err)))))

(defun put-kv-str-raw (db key val &optional (opts (rocksdb-writeoptions-create)))
  (let ((key-octets (string-to-octets key :null-terminate nil))
        (val-octets (string-to-octets val :null-terminate nil)))
    (put-kv-raw db key-octets val-octets opts)))

(defun put-cf-raw (db cf key val &optional (opts (rocksdb-writeoptions-create)))
  (let ((klen (length key))
	(vlen (length val)))
    (with-errptr (err 'put-kv-error (list :db db :kv (cons key val)))
      (with-alien ((k (* char) (make-alien char klen))
		   (v (* char) (make-alien char vlen)))
        (setfa k key)
        (setfa v val)
        (rocksdb-put-cf db
		        opts
                        cf
		        k klen
		        v vlen
		        err)))))

(defun put-cf-str-raw (db cf key val &optional (opt (rocksdb-writeoptions-create)))
  (let ((key-octets (string-to-octets key :null-terminate nil))
        (val-octets (string-to-octets val :null-terminate nil)))
    (put-cf-raw db cf key-octets val-octets opt)))

(defun get-kv-raw (db key &optional (opt (rocksdb-readoptions-create)))
  (let ((klen (length key)))
    (with-errptr (err 'get-kv-error (list :db db :key key))
      (with-alien ((vlen (* size-t) (make-alien size-t 0))
		   (k (* char) (make-alien char klen)))
        (setfa k key)
        (let* ((val (rocksdb-get db
			         opt
			         k klen
                                 vlen
			         err)))
	  ;; helps if we know the vlen beforehand, would need a custom
	  ;; C-side function probably.
	  (let ((v (make-array (deref vlen) :element-type 'unsigned-byte)))
            (clone-octets-from-alien val v (deref vlen))
	    v))))))

(defun get-kv-str-raw (db key &optional (opt (rocksdb-readoptions-create)))
  (let ((k (string-to-octets key :null-terminate nil)))
    (let ((v (get-kv-raw db k opt)))
      (when v (concatenate 'string (map 'vector #'code-char v))))))

(defun get-cf-raw (db cf key &optional (opt (rocksdb-readoptions-create)))
  (let ((klen (length key)))
    (with-errptr (err 'get-kv-error (list :db db :key key))
      (with-alien ((vlen (* size-t) (make-alien size-t 0))
		   (k (* char) (make-alien char klen)))
        (setfa k key)
        (let* ((val (rocksdb-get-cf db
			            opt
                                    cf
			            k klen
                                    vlen
			            err)))
	  ;; helps if we know the vlen beforehand, would need a custom
	  ;; C-side function probably.
	  (let ((v (make-array (deref vlen) :element-type 'unsigned-byte)))
            (clone-octets-from-alien val v (deref vlen))
	    v))))))

(defun get-cf-str-raw (db cf key &optional (opt (rocksdb-readoptions-create)))
  (let ((k (string-to-octets key :null-terminate nil)))
    (let ((v (get-cf-raw db cf k opt)))
      (when v (concatenate 'string (map 'vector #'code-char v))))))

;;; Column Family
(defun create-cf-raw (db name &optional (opt (rocksdb-options-create)))
  (with-errptr (err 'rocksdb-cf-error (list :db db :cf name)) 
    (rocksdb-create-column-family db opt name err)))

;;; Iterators
(defun create-iter-raw (db &optional (opt (rocksdb-readoptions-create)))
  (rocksdb-create-iterator db opt))

(defun iter-key-raw (iter)
  (with-alien ((klen-ptr (* size-t) (make-alien size-t 0)))
    (let* ((key-ptr (rocksdb-iter-key iter klen-ptr))
           (klen (deref klen-ptr))
           (k (make-array klen :element-type '(unsigned-byte 8))))
      (clone-octets-from-alien key-ptr k klen)
      k)))

(defun iter-key-str-raw (iter)
  (when-let ((k (iter-key-raw iter)))
    (octets-to-string k)))

(defun iter-val-raw (iter)
  (with-alien ((vlen-ptr (* size-t) (make-alien size-t 0)))
    (let* ((val-ptr (rocksdb-iter-value iter vlen-ptr))
           (vlen (deref vlen-ptr))
           (v (make-array vlen :element-type '(unsigned-byte 8))))
      (clone-octets-from-alien val-ptr v vlen)
      v)))

(defun iter-val-str-raw (iter)
  (when-let ((v (iter-val-raw iter)))
    (octets-to-string v)))

(defmacro with-iter-raw ((iter-var db &optional (opt (rocksdb-readoptions-create))) &body body)
  `(let ((,iter-var (create-iter-raw ,db ,opt)))
     (unwind-protect (progn ,@body)
       (rocksdb-iter-destroy ,iter-var))))

;;; Backup Engine
(defun open-backup-engine-raw (be-path &optional (opts (rocksdb-options-create)))
  (with-errptr (err 'open-backup-engine-error (list :db be-path))
    (let ((be-path (if (pathnamep be-path)
                       (namestring be-path)
                       be-path)))
      (rocksdb-backup-engine-open opts be-path err))))

(defun close-backup-engine-raw (be)
  (rocksdb-backup-engine-close be))

(defun create-new-backup-raw (be db)
  (with-errptr (err 'rocksdb-error)
    (rocksdb-backup-engine-create-new-backup be db err)))

(defun restore-from-latest-backup-raw (be db-path be-path &optional (opt (rocksdb-restore-options-create)))
  (with-errptr (err 'rocksdb-error)
    (rocksdb-backup-engine-restore-db-from-latest-backup be db-path be-path opt err)))

(defmacro with-open-backup-engine-raw ((be-var be-path &optional (opt (rocksdb-options-create)))
                                       &body body)
  `(let ((,be-var (open-backup-engine-raw ,be-path ,opt)))
     (unwind-protect (progn ,@body)
       (rocksdb-backup-engine-close ,be-var))))
