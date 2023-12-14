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
   (lambda (o) (rocksdb-options-set-create-if-missing o t))))

(defun open-db-raw (db-path opts)
  (with-errptr err
    (let* ((db-path (if (pathnamep db-path)
                        (namestring db-path)
                        db-path))
           (db (rocksdb-open opts db-path err))
           (err (deref err)))
      (handle-errptr err 'open-db-error (list :db db-path))
      db)))

(defun close-db-raw (db)
  (rocksdb-close db))

(defun destroy-db-raw (path)
  (with-errptr err
    (let ((opt (rocksdb-options-create)))
      (unwind-protect (rocksdb-destroy-db opt path err)
        (rocksdb-options-destroy opt)
        (handle-errptr err 'destroy-db-error (list :db path))))))

;;(with-open-db-raw (db "/tmp/tmp-db"))
;; (destroy-db-raw "/tmp/with-db-raw")

(defmacro with-open-db-raw ((db-var db-path &optional (opt (default-rocksdb-options))) &body body)
  `(let ((,db-var (open-db-raw ,db-path ,opt)))
     (unwind-protect (progn ,@body)
       (rocksdb-close ,db-var)
       (with-errptr err
         (rocksdb-destroy-db ,opt ,db-path err)
         (rocksdb-options-destroy ,opt)
         (handle-errptr err 'rocksdb-error)))))

(defun put-kv-raw (db key val &optional (opts (rocksdb-writeoptions-create)))
  (let ((klen (length key))
	(vlen (length val)))

    (with-alien ((k (* char) (make-alien char klen))
		 (v (* char) (make-alien char vlen)))
      (setfa k key)
      (setfa v val)
      (with-errptr err
        (rocksdb-put db
		     opts
		     k
		     klen
		     v
		     vlen
		     err)
        (handle-errptr err 'put-kv-error (list :db db :kv (cons key val)))))))

(defun put-kv-str-raw (db key val &optional opt)
  (let ((key-octets (string-to-octets key))
        (val-octets (string-to-octets val)))
    (put-kv-raw db key-octets val-octets opt)))

(defun put-cf-raw (db cf key val &optional (opts (rocksdb-writeoptions-create)))
  (let ((klen (length key))
	(vlen (length val)))
    (with-errptr err
      (with-alien ((k (* char) (make-alien char klen))
		   (v (* char) (make-alien char vlen)))
        (setfa k key)
        (setfa v val)
        (rocksdb-put-cf db
		        opts
                        cf
		        k
		        klen
		        v
		        vlen
		        err)
        (handle-errptr err 'put-kv-error (list :db db :kv (cons key val)))))))

(defun put-cf-str-raw (db cf key val &optional opt)
  (let ((key-octets (string-to-octets key))
        (val-octets (string-to-octets val)))
    (put-cf-raw db cf key-octets val-octets opt)))

(defun get-kv-raw (db key &optional (opt (rocksdb-readoptions-create)))
  (let ((klen (length key)))
    (with-errptr err
      (with-alien ((vlen (* size-t) (make-alien size-t 0))
		   (err rocksdb-errptr)
		   (k (* char) (make-alien char klen)))
        (setfa k key)
        (let* ((val (rocksdb-get db
			         opt
			         k
			         klen
                                 vlen
			         err)))
          (handle-errptr err 'get-kv-error (list :db db :key key))
	  ;; helps if we know the vlen beforehand, would need a custom
	  ;; C-side function probably.
	  (let ((v (make-array (deref vlen) :element-type 'unsigned-byte)))
            (clone-octets-from-alien val v (deref vlen))
	    v))))))

(defun get-kv-str-raw (db key &optional opt)
  (let ((k (string-to-octets key)))
    (let ((v (get-kv-raw db k opt)))
      (when v (concatenate 'string (map 'vector #'code-char v))))))

(defun get-cf-raw (db cf key &optional (opt (rocksdb-readoptions-create)))
  (let ((klen (length key)))
    (with-errptr err
      (with-alien ((vlen (* size-t) (make-alien size-t 0))
		   (k (* char) (make-alien char klen)))
        (setfa k key)
        (let* ((val (rocksdb-get-cf db
			            opt
                                    cf
			            k
			            klen
                                    vlen
			            err)))
          (handle-errptr err 'get-kv-error (list :db db :key key))
	  ;; helps if we know the vlen beforehand, would need a custom
	  ;; C-side function probably.
	  (let ((v (make-array (deref vlen) :element-type 'unsigned-byte)))
            (clone-octets-from-alien val v (deref vlen))
	    v))))))

(defun get-cf-str-raw (db cf key &optional opt)
  (let ((k (string-to-octets key)))
    (let ((v (get-cf-raw db cf k opt)))
      (when v (concatenate 'string (map 'vector #'code-char v))))))

(defun create-iter (db &optional (opt (rocksdb-readoptions-create)))
  (rocksdb-create-iterator db opt))

(defun iter-key (iter)
  (with-alien ((klen-ptr (* size-t) (make-alien size-t 0)))
    (let* ((key-ptr (rocksdb-iter-key iter klen-ptr))
           (klen (deref klen-ptr))
           (k (make-array klen :element-type '(unsigned-byte 8))))
      (clone-octets-from-alien key-ptr k klen)
      k)))

(defun iter-key-str (iter)
  (when-let ((k (iter-key iter)))
    (octets-to-string k)))

(defun iter-val (iter)
  (with-alien ((vlen-ptr (* size-t) (make-alien size-t 0)))
    (let* ((val-ptr (rocksdb-iter-value iter vlen-ptr))
           (vlen (deref vlen-ptr))
           (v (make-array vlen :element-type '(unsigned-byte 8))))
      (clone-octets-from-alien val-ptr v vlen)
      v)))

(defun iter-val-str (iter)
  (when-let ((v (iter-val iter)))
    (octets-to-string v)))
