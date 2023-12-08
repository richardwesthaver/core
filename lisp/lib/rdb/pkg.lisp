;;; rdb.lisp --- High-level RocksDB API

;; a thin ORM for working with RocksDB storage. 

;; Low-level bindings are in rocksdb.lisp.

;; Commentary:

;; Code:
(uiop:define-package :rdb/pkg
  (:nicknames :rdb)
  (:use :cl :std/alien :std/fu :std/sym :rocksdb)
  (:import-from :sb-ext :string-to-octets :octets-to-string)
  (:export 
   ;; opts
   :rdb-opts :make-rdb-opts
   :default-rdb-opts
   ;; db
   :open-db :with-open-db 
   :close-db :destroy-db
   ;; cfs
   :rdb-cf :make-rdb-cf
   :with-cf
   ;; ops
   :put-kv :put-kv-str
   :get-kv :get-kv-str
   ;; iter
   :create-iter :with-iter
   :iter-key :iter-key-str
   :iter-val :iter-val-str
   ;; err
   :open-db-error
   :put-kv-error
   :get-kv-error))

(in-package :rdb/pkg)

(defmacro with-errptr (e &body body)
    `(with-alien ((,e rocksdb-errptr))
       ,@body))

(defstruct rdb-opts
  (create-if-missing nil :type boolean)
  (total-threads 1 :type integer) ;; numcpus is default
  (max-open-files 10000 :type integer)
  (use-fsync nil :type boolean)
  (destroy nil :type boolean) ;; *
  (disable-auto-compactions nil :type boolean))

(defstruct rdb-cf
  (name "" :type string))

(defun create-cf (db cf)
  (with-errptr err
    (rocksdb-create-column-family db (rocksdb-options-create) (rdb-cf-name cf) err)))

(defmacro with-cf ((cf-var cf) &body body)
  `(let ((,cf-var ,cf))
    ,@body))

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

(defun open-db (db-path &optional opts)
  (let ((opts (if opts (bind-rocksdb-opts% opts) (default-rocksdb-options%))))
    (with-alien ((e rocksdb-errptr))
      (let* ((db-path (if (pathnamep db-path)
                          (namestring db-path)
                          db-path))
             (db (rocksdb-open opts db-path e))
             (err e))
	(unless (null-alien err)
          (error 'open-db-error
                 :db-path db-path
                 :error-message e))
        db))))

(defun close-db (db)
  (rocksdb-close db))

(defun destroy-db (path)
  (with-alien ((err rocksdb-errptr))
    (rocksdb-destroy-db (rocksdb-options-create) path err)))

(defmacro with-open-db ((db-var db-path &optional opt) &body body)
  `(let ((,db-var (open-db ,db-path ,opt)))
     (unwind-protect (progn ,@body)
       (close-db ,db-var)
       (when (and ,opt (rdb-opts-destroy ,opt))
         (destroy-db ,db-path)))))

(defmacro with-iter ((iter-var db &optional opt) &body body)
  `(let ((,iter-var (create-iter ,db ,opt)))
     (unwind-protect (progn ,@body)
       (rocksdb-iter-destroy ,iter-var))))

;;; Conditions
(define-condition open-db-error (error)
  ((db-path :initarg :db-path
            :reader db-path)
   (error-message :initarg :error-message
                  :reader error-message)))

(defmethod print-object ((obj open-db-error) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "error-message=~A" (error-message obj))))

(define-condition put-kv-error (error)
  ((db :initarg :db
       :reader db)
   (key :initarg :key
        :reader key)
   (val :initarg :val
        :reader val)
   (error-message :initarg :error-message
                  :reader error-message)))

(define-condition get-kv-error (error)
  ((db :initarg :db
       :reader db)
   (key :initarg :key
        :reader key)
   (error-message :initarg :error-message
                  :reader error-message)))

(defun put-kv (db key val &optional opts)
  (let ((opts (or opts (rocksdb-writeoptions-create)))
	(klen (length key))
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

(defun put-kv-str (db key val &optional opt)
  (let ((key-octets (string-to-octets key))
        (val-octets (string-to-octets val)))
    (put-kv db key-octets val-octets opt)))

(defun get-kv (db key &optional opt)
  (let ((opt (or opt (rocksdb-readoptions-create)))
	(klen (length key)))
    (with-alien ((vlen (* size-t) (make-alien size-t 0))
		 (errptr rocksdb-errptr nil)
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
		 :error-message (alien-sap errptr)))
	;; helps if we know the vlen beforehand, would need a custom
	;; C-side function probably.
	(let ((v (make-array (deref vlen) :element-type 'unsigned-byte)))
          (clone-octets-from-alien val v (deref vlen))
	  v)))))

(defun get-kv-str (db key &optional opt)
   (let ((k (string-to-octets key)))
     (let ((v (get-kv db k opt)))
       (when v (concatenate 'string (map 'vector #'code-char v))))))

(defun create-iter (db &optional opt)
  (unless opt
    (setq opt (rocksdb-readoptions-create)))
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
