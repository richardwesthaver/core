;;; rdb/condition.lisp --- RDB conditions

;;

;;; Code:
(in-package :rdb)

(eval-always
  (deferror rdb-error ()
      ((message :initarg :message
                :reader error-message))
      (:auto t)
      (:documentation "Error signaled by the RDB system.")))

(define-condition rdb-alien-error (rdb-error rocksdb-c-error)
  ((db :initarg :db :reader error-db))
  (:documentation "Error signaled by RDB C subsystem."))

(defmethod print-object ((obj rdb-error) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A" (rdb-error-message obj))))

(define-condition open-db-error (rdb-alien-error)
  ()
  (:documentation "Error signaled while opening a database."))

(define-condition open-backup-engine-error (rdb-alien-error)
  ()
  (:documentation "Error signaled while opening a backup engine."))

(define-condition destroy-db-error (rdb-alien-error)
  ()
  (:documentation "Error signaled while destroying a database."))

(define-condition flush-db-error (rdb-alien-error)
  ()
  (:documentation "Error signaled while flushing a database."))

(define-condition ingest-db-error (rdb-alien-error)
  ()
  (:documentation "Error signaled while ingesting a database."))

(define-condition sst-writer-error (rdb-alien-error)
  ()
  (:documentation "Error signaled while writing a SST file."))

(define-condition repair-db-error (rdb-alien-error)
  ()
  (:documentation "Error signaled while repairing a database."))

(define-condition destroy-backup-engine-error (rdb-alien-error)
  ()
  (:documentation "Error signaled while destroying a backup engine."))

(define-condition cf-error (rdb-alien-error)
  ((cf :initarg :cf :reader error-cf))
  (:documentation "Error signaled in the context of a Column Family."))

(define-condition put-kv-error (rdb-error)
  ((kv :initarg :kv :reader error-kv))
  (:documentation "Error signaled while processing a PUT-KV request"))

(define-condition get-kv-error (rdb-error)
  ((key :initarg :key :reader error-key))
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
