;;; rdb.lisp --- High-level RocksDB API

;; a thin ORM for working with RocksDB storage. 

;; Low-level bindings are in rocksdb.lisp.

;; Commentary:

;; Code:
(defpackage :rdb
  (:use :cl :rocksdb :sb-alien :alien)
  (:import-from :sb-ext :string-to-octets :octets-to-string)
  (:export :with-open-db :with-iter
           :create-iter
           :iter-key :iter-key-str
           :iter-val :iter-val-str
   :unable-to-open-db :unable-to-put-key-value-to-db :unable-to-get-value-to-db))

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
(defun open-db (db-path &optional opt)
  (unless opt
    (setq opt (rocksdb-options-create)))
  (with-alien ((e rocksdb-errptr nil))
    (let* ((db-path (if (pathnamep db-path)
                        (namestring db-path)
                        db-path))
           (db (rocksdb-open opt db-path e)))
      (if (null-alien e)
          db
          (error 'unable-to-open-db
                 :db-path db-path
                 :error-message e)))))

;; (defmacro clone-octets-to-foreign (lisp-array foreign-array)
;;   (let ((i (gensym)))
;;     `(loop for ,i from 0 below (length ,lisp-array)
;;            do (setf (deref ,foreign-array ,i)
;;                     (aref ,lisp-array ,i)))))

;; (defmacro clone-octets-from-foreign (foreign-array lisp-array len)
;;   (let ((i (gensym)))
;;     `(loop for ,i from 0 below ,len
;;            do (setf (aref ,lisp-array ,i)
;;                     (deref ,foreign-array ,i)))))

;; (defun put-kv (db key val &optional opt)
;;   (unless opt
;;     (setq opt (create-writeoptions)))
;;   (with-alien ((errptr (* t))
;;                          (key* unsigned-char (length key))
;;                          (val* unsigned-char (length val)))
;;     (clone-octets-to-foreign key key*)
;;     (clone-octets-to-foreign val val*)
;;     (put* db
;;           opt
;;           key*
;;           (length key)
;;           val*
;;           (length val)
;;           errptr)
;;     (let ((err errptr))
;;       (unless (null-alien err)
;;         (error 'unable-to-put-key-value-to-db
;;                :db db
;;                :key key
;;                :val val
;;                :error-message (sap-alien err c-string))))))

;; (defun put-kv-str (db key val &optional opt)
;;   (let ((key-octets (string-to-octets key))
;;         (val-octets (string-to-octets val)))
;;     (put-kv db key-octets val-octets opt)))

;; (defun get-kv (db key &optional opt)
;;   (unless opt
;;     (setq opt (create-readoptions)))

;;   (with-alien ((val-len-ptr unsigned-int)
;;                (errptr system-area-pointer)
;;                (key* unsigned-char (length key)))
;;     (clone-octets-to-foreign key key*)
;;     ;; (setf (mem-ref errptr :pointer) (null-pointer))
;;     (let ((val (get* db
;;                      opt
;;                      key*
;;                      (length key)
;;                      val-len-ptr
;;                      errptr)))
;;       (let ((err errptr))
;;         (unless (null-alien err)
;;           (error 'unable-to-get-value-to-db
;;                  :db db
;;                  :key key
;;                  :error-message (sap-alien err c-string)))
        
;;         (unless (null-alien val)
;;           (let* ((val-len val-len-ptr)
;;                  (val* (make-array val-len
;;                                       :element-type '(unsigned-byte 8))))
;;             (clone-octets-from-foreign val val* val-len)
;;             val*))))))

;; (defun get-kv-str (db key &optional opt)
;;   (let ((key-octets (string-to-octets key)))
;;     (let ((#1=val-octets (get-kv db key-octets opt)))
;;       (when #1#
;;         (octets-to-string #1#)))))

(defun create-iter (db &optional opt)
  (unless opt
    (setq opt (rocksdb-readoptions-create)))
  (rocksdb-create-iterator db opt))

;; (defun iter-key (iter)
;;   (with-alien ((klen-ptr unsigned-int 0))
;;     (let* ((key-ptr (rocksdb-iter-key iter klen-ptr))
;;            (klen klen-ptr)
;;            (key (make-array klen :element-type '(unsigned-byte 8))))
;;       (clone-octets-from-foreign key-ptr key klen)
;;       key)))

;; (defun iter-key-str (iter)
;;   (when-let ((key-octets (iter-key iter)))
;;     (octets-to-string key-octets)))

;; (defun iter-val (iter)
;;   (with-alien ((len-ptr unsigned-int 0))
;;     (let* ((value-ptr (rocksdb-iter-value iter len-ptr))
;;            (vlen len-ptr)
;;            (value* (make-array vlen :element-type '(unsigned-byte 8))))
;;       (clone-octets-from-foreign value-ptr value* vlen)
;;       value*)))

;; (defun iter-val-str (iter)
;;   (let ((#1=val-octets (iter-value iter)))
;;     (when #1#
;;       (octets-to-string #1#))))
