(in-package :rdb)

(defmacro with-errptr (e &body body)
  `(progn
     (with-alien ((,e (* (* t)) (make-alien rocksdb-errptr)))
       ,@body)))

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
  (declare (type alien errptr))
  ;; first we dereference (* (* t)) -> (* t)
  (let ((err (deref errptr)))
    ;; if NULL, return nil
    (unless (null-alien err)
      ;; cast the non-NULL pointer to a string
      (let ((msg (cast err c-string)))
        (apply #'signal (or errtyp 'rdb-error)
               (nconc `(:message ,msg) params))))))
