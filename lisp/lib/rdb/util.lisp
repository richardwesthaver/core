(in-package :rdb)

(defmacro with-errptr (e &body body)
  `(progn
     (with-alien ((,e (* (* t)) (make-alien rocksdb-errptr)))
       ,@body)))

(defun default-rocksdb-options ()
  (let ((opts (rocksdb-options-create)))
    (rocksdb-options-set-create-if-missing opts t)
    opts))

(defun open-db-raw (db-path opts)
  (with-errptr err
    (let* ((db-path (if (pathnamep db-path)
                        (namestring db-path)
                        db-path))
           (db (rocksdb-open opts db-path err))
           (err (deref err)))
      (unless (null-alien err)
        (error 'open-db-error
               :db-path db-path
               :error-message (cast err c-string)))
      db)))

(defun close-db-raw (db)
  (rocksdb-close db))

(defun destroy-db-raw (path)
  (let ((opt (rocksdb-options-create)))
    (with-alien ((err rocksdb-errptr nil))
      (rocksdb-destroy-db opt path err))
      (rocksdb-options-destroy opt)))

(defmacro with-open-db-raw ((db-var db-path &optional (opt (default-rocksdb-options))) &body body)
  `(let ((,db-var (open-db-raw ,db-path ,opt)))
     (unwind-protect (progn ,@body)
       (rocksdb-close ,db-var)
       (with-errptr err
         (rocksdb-destroy-db ,opt ,db-path err))
       (rocksdb-options-destroy ,opt))))

(defun put-kv-raw (db key val &optional (opts (rocksdb-writeoptions-create)))
  (let ((klen (length key))
	(vlen (length val)))
    (with-alien ((k (* char) (make-alien char klen))
		 (v (* char) (make-alien char vlen))
                 (errptr rocksdb-errptr nil))
      (setfa k key)
      (setfa v val)
      (rocksdb-put db
		   opts
		   k
		   klen
		   v
		   vlen
		   errptr)
      (unless (null-alien errptr)
        (error 'put-kv-error
                :db db
                :key key
                :val val
                :error-message (alien-sap errptr))))))

(defun put-kv-str-raw (db key val &optional opt)
  (let ((key-octets (string-to-octets key))
        (val-octets (string-to-octets val)))
    (put-kv-raw db key-octets val-octets opt)))

(defun put-cf-raw (db cf key val &optional (opts (rocksdb-writeoptions-create)))
  (let ((klen (length key))
	(vlen (length val)))
    (with-alien ((k (* char) (make-alien char klen))
		 (v (* char) (make-alien char vlen))
                 (errptr rocksdb-errptr nil))
      (setfa k key)
      (setfa v val)
      (rocksdb-put-cf db
		      opts
                      cf
		      k
		      klen
		      v
		      vlen
		      errptr)
      (unless (null-alien errptr)
        (error 'put-kv-error
                :db db
                :key key
                :val val
                :error-message (alien-sap errptr))))))

(defun put-cf-str-raw (db cf key val &optional opt)
  (let ((key-octets (string-to-octets key))
        (val-octets (string-to-octets val)))
    (put-cf-raw db cf key-octets val-octets opt)))

(defun get-kv-raw (db key &optional (opt (rocksdb-readoptions-create)))
  (let ((klen (length key)))
    (with-alien ((vlen (* size-t) (make-alien size-t 0))
		 (errptr rocksdb-errptr)
		 (k (* char) (make-alien char klen)))
      (setfa k key)
      (let* ((val (rocksdb-get db
			       opt
			       k
			       klen
                               vlen
			       errptr)))
	(unless (null-alien errptr)
          (error 'get-kv-error
		 :db db
		 :key key
		 :error-message errptr))
	;; helps if we know the vlen beforehand, would need a custom
	;; C-side function probably.
	(let ((v (make-array (deref vlen) :element-type 'unsigned-byte)))
          (clone-octets-from-alien val v (deref vlen))
	  v)))))

(defun get-kv-str-raw (db key &optional opt)
   (let ((k (string-to-octets key)))
     (let ((v (get-kv-raw db k opt)))
       (when v (concatenate 'string (map 'vector #'code-char v))))))

(defun get-cf-raw (db cf key &optional (opt (rocksdb-readoptions-create)))
  (let ((klen (length key)))
    (with-alien ((vlen (* size-t) (make-alien size-t 0))
		 (errptr rocksdb-errptr nil)
		 (k (* char) (make-alien char klen)))
      (setfa k key)
      (let* ((val (rocksdb-get-cf db
			          opt
                                  cf
			          k
			          klen
                                  vlen
			          errptr)))
	(unless (null-alien errptr)
          (error 'get-kv-error
		 :db db
		 :key key
		 :error-message (alien-sap errptr)))
	;; helps if we know the vlen beforehand, would need a custom
	;; C-side function probably.
	(let ((v (make-array (deref vlen) :element-type 'unsigned-byte)))
          (clone-octets-from-alien val v (deref vlen))
	  v)))))

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

(defmacro with-iter ((iter-var db &optional opt) &body body)
  `(let ((,iter-var (create-iter ,db ,opt)))
     (unwind-protect (progn ,@body)
       (rocksdb-iter-destroy ,iter-var))))
