;;; rdb/err.lisp --- RDB errors

;;

;;; Code:
(in-package :rdb)

(eval-always
  (deferror rdb-error ()
      ((message :initarg :message
                :reader rdb-error-message))
      (:auto t)
      (:documentation "Error signaled by the RDB system.")))

(define-condition rocksdb-error (rdb-error)
  ((db :initarg :db :reader rdb-error-db))
  (:documentation "Error signaled by RocksDB subsystem."))

(defmethod print-object ((obj rdb-error) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A" (rdb-error-message obj))))

(define-condition open-db-error (rocksdb-error)
  ()
  (:documentation "Error signaled while opening a database."))

(define-condition open-backup-engine-error (rocksdb-error)
  ()
  (:documentation "Error signaled while opening a backup engine."))

(define-condition destroy-db-error (rocksdb-error)
  ()
  (:documentation "Error signaled while destroying a database."))

(define-condition flush-db-error (rocksdb-error)
  ()
  (:documentation "Error signaled while flushing a database."))

(define-condition repair-db-error (rocksdb-error)
  ()
  (:documentation "Error signaled while repairing a database."))

(define-condition destroy-backup-engine-error (rocksdb-error)
  ()
  (:documentation "Error signaled while destroying a backup engine."))

(define-condition cf-error (rocksdb-error)
  ((cf :initarg :cf :reader rdb-error-cf))
  (:documentation "Error signaled in the context of a Column Family."))

(define-condition put-kv-error (rdb-error)
  ((kv :initarg :kv :reader rdb-error-kv))
  (:documentation "Error signaled while processing a PUT-KV request"))

(define-condition get-kv-error (rdb-error)
  ((key :initarg :key :reader key))
  (:documentation "Error signaled while processing a GET-KV request"))

(define-condition opt-handler-missing (warning rdb-error)
  ())

(define-condition db-missing (warning rdb-error)
  ())

(define-condition metadata-missing (warning rdb-error)
  ())

(define-condition invalid-propname (rdb-error)
  ()
  (:documentation "Error signaled when an invalid ROCKSDB-PROPERTY value is detected."))

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
