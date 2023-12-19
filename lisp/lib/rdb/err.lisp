(in-package :rdb)

(define-condition rdb-error (error)
  ((message :initarg :message
            :reader rdb-error-message))
  (:documentation "Error signaled by the RDB system"))

(define-condition rocksdb-error (rdb-error)
  ((db :initarg :db :reader rdb-error-db))
  (:documentation "Error signaled by RocksDB subsystem"))

(defmethod print-object ((obj rdb-error) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A" (rdb-error-message obj))))

(define-condition open-db-error (rocksdb-error)
  ()
  (:documentation "Error signaled while opening a database"))

(define-condition destroy-db-error (rocksdb-error)
  ()
  (:documentation "Error signaled while destroying a database"))

(define-condition cf-error (rocksdb-error)
  ((cf :initarg :cf :reader rdb-error-cf)))

(define-condition put-kv-error (rdb-error)
  ((kv :initarg :kv :reader rdb-error-kv))
  (:documentation "Error signaled while processing a PUT-KV request"))

(define-condition get-kv-error (rdb-error)
  ((key :initarg :key :reader key))
  (:documentation "Error signaled while processing a GET-KV request"))

(defun handle-errptr (errptr &optional errtyp params)
  "Handle ERRPTR, a ROCKSDB-ERRPTR type which is a pointer to NULL,
indicating a success or a pointer to a C-STRING.

ERRTYP if present must be a condition which sub-classes RDB-ERROR. If
an error is detected, the resulting string from ERRPTR and the
additional PARAMS will be used to signal a lisp error condition."
  ;; if NULL, return nil
  (unless (null-alien errptr)
    (apply #'signal (or errtyp 'rdb-error)
           (nconc (list :message errptr) params))))

(defmacro with-errptr ((e &optional errtyp params) &body body)
  `(with-alien ((,e rocksdb-errptr nil))
     (unwind-protect 
          (handler-bind ((sb-sys:memory-fault-error 
                           (lambda (condition)
                             (error 'rdb-error
                                    :message
                                    (format nil
                                            "~a" condition))))
                         (error 
                           (lambda (condition)
                             (error 'rdb-error 
                                    :message 
                                    (format nil 
                                            "unhandled exception in body of WITH-ERRPTR: ~a"
                                            condition)))))
            (progn ,@body))
       (handle-errptr ,e ,errtyp ,params))))
        
          
