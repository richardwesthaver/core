;;; rdb.lisp --- High-level RocksDB API

;; a thin ORM for working with RocksDB storage. 

;; Low-level bindings are in rocksdb.lisp.

;; Commentary:

;; Code:
(uiop:define-package :rdb/pkg
  (:nicknames :rdb)
  (:use :cl :std/alien :std/fu :rocksdb)
  (:import-from :sb-ext :string-to-octets :octets-to-string)
  (:reexport :rocksdb)
  (:export 
   ;; opts
   :make-rdb-opts
   :rdb-opts
   :default-rdb-opts
   ;; db
   :open-db
   :with-open-db 
   ;; iter
   :create-iter :with-iter
   :iter-key :iter-key-str
   :iter-val :iter-val-str
   ;; err
   :unable-to-open-db 
   :unable-to-put-key-value-to-db 
   :unable-to-get-value-to-db))

(in-package :rdb/pkg)

(defstruct rdb-opts
  (create-if-missing nil :type boolean)
  (total-threads 1 :type integer) ;; numcpus is default
  (max-open-files 10000 :type integer)
  (use-fsync nil :type boolean)
  (disable-auto-compactions nil :type boolean))

;; unsafe
(defun bind-rocksdb-opts% (opts)
  (let ((o (rocksdb-options-create)))
    (with-slots (create-if-missing total-threads) opts
      (rocksdb-options-set-create-if-missing o create-if-missing)
      (rocksdb-options-increase-parallelism o total-threads))
    o))

(defun default-rdb-opts () 
  (make-rdb-opts
   :create-if-missing t 
   :total-threads 4))

(defun default-rocksdb-options% ()
  (bind-rocksdb-opts% (default-rdb-opts)))

(defmacro with-open-db ((db-var db-path &optional opt) &body body)
  `(let ((,db-var (open-db ,db-path ,opt)))
     (unwind-protect (progn ,@body)
       (rocksdb-close ,db-var))))

(defmacro with-iter ((iter-var db &optional opt) &body body)
  `(let ((,iter-var (create-iter ,db ,opt)))
     (unwind-protect (progn ,@body)
       (rocksdb-iter-destroy ,iter-var))))

;;; Conditions
(define-condition unable-to-open-db (error)
  ((db-path :initarg :db-path
            :reader db-path)
   (error-message :initarg :error-message
                  :reader error-message)))

(defmethod print-object ((obj unable-to-open-db) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "error-message=~A" (error-message obj))))

(define-condition unable-to-put-key-value-to-db (error)
  ((db :initarg :db
       :reader db)
   (key :initarg :key
        :reader key)
   (val :initarg :val
        :reader val)
   (error-message :initarg :error-message
                  :reader error-message)))

(define-condition unable-to-get-value-to-db (error)
  ((db :initarg :db
       :reader db)
   (key :initarg :key
        :reader key)
   (error-message :initarg :error-message
                  :reader error-message)))

;;; API
(defun open-db (db-path &optional opts)
  (let ((opts (if opts (bind-rocksdb-opts% opts) (default-rocksdb-options%))))
    (with-alien ((e rocksdb-errptr))
      (let* ((db-path (if (pathnamep db-path)
                          (namestring db-path)
                          db-path))
             (db (rocksdb-open opts db-path e)))
	(if (null-alien e)
            db
            (error 'unable-to-open-db
                   :db-path db-path
                   :error-message e))))))

(defun put-kv (db key val &optional opts)
  (let ((opts (or opts (rocksdb-writeoptions-create)))
	(klen (length key))
	(vlen (length val)))
    (with-alien ((errptr rocksdb-errptr nil)
		 (k (* char) (make-alien char klen))
		 (v (* char) (make-alien char vlen)))
      (loop for x across key
	    for i from 0 below klen
	    do (setf (deref k i) x))
      (loop for y across val
	    for i from 0 below vlen
	    do (setf (deref v i) y))
      (rocksdb-put db
		   opts
		   k
		   klen
		   v
		   vlen
		   errptr)
      (unless (null-alien errptr)
        (error 'unable-to-put-key-value-to-db
                :db db
                :key key
                :val val
                :error-message (alien-sap errptr))))))

(defun put-kv-str (db key val &optional opt)
  (let ((key-octets (string-to-octets key))
        (val-octets (string-to-octets val)))
    (put-kv db key-octets val-octets opt)))

(defun get-kv (db key &optional opt)
  (let ((opt (or opt (rocksdb-readoptions-create)))
	(key (string-to-octets key))
	(klen (length key)))
    (with-alien ((vlen (* size-t))
		 (errptr rocksdb-errptr nil)
		 (k (* char) (make-alien char klen)))
      (loop for x across key
	    for i from 0 below klen
	    do (setf (deref k i) x))

      (let* ((val (rocksdb-get db
			      opt
			      k
			      klen
			      vlen
			      errptr))
	     (vlen (deref vlen)))
	(unless (null-alien errptr)
          (error 'unable-to-get-value-to-db
		 :db db
		 :key key
		 :error-message (alien-sap errptr)))
	;; helps if we know the vlen beforehand, would need a custom
	;; C-side function probably.
	(let ((v (make-array vlen :element-type 'unsigned-byte)))
	  (loop for i from 0 below vlen
		with x = (deref val i) 
		do (setf (aref v i) x))
	  (map 'vector #'code-char v))))))

 (defun get-kv-str (db key &optional opt)
   (let ((k (string-to-octets key)))
     (let ((v (get-kv db k opt)))
       (when v (print v)))))

(defun create-iter (db &optional opt)
  (unless opt
    (setq opt (rocksdb-readoptions-create)))
  (rocksdb-create-iterator db opt))

(defun iter-key (iter)
  (with-alien ((klen-ptr (* unsigned-int)))
    (let* ((key-ptr (rocksdb-iter-key iter klen-ptr))
           (klen (deref klen-ptr))
           (k (make-array klen :element-type '(unsigned-byte 8))))
      (loop for i from 0 below klen with x = (deref key-ptr i) do (setf (aref k i) x))
      k)))

(defun iter-key-str (iter)
  (when-let ((k (iter-key iter)))
    (octets-to-string k)))

 (defun iter-val (iter)
   (with-alien ((vlen-ptr (* unsigned-int)))
     (let* ((val-ptr (rocksdb-iter-value iter vlen-ptr))
            (vlen (deref vlen-ptr))
            (v (make-array vlen :element-type '(unsigned-byte 8))))
       (loop for i from 0 below vlen with x = (deref val-ptr i) do (setf (aref v i) x))
       v)))

 (defun iter-val-str (iter)
   (when-let ((v (iter-val iter)))
     (octets-to-string v)))
