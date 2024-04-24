;;; rdb/sst.lisp --- Sorted Sequence Tables

;; SST is the on-disk format for RocksDB data. Keys are (usually)
;; sorted. An SST-FILE can be generated independently of a database
;; and ingested on-demand.

;;; Commentary:

;; This file defines the high-level SST-FILE-WRITER object which
;; implements the relevant protocols for generating SST Files on the
;; fly. To 'read' an SST file you should ingest it into an open
;; database 

;;; Code:
(in-package :rdb)

(defstruct (sst-file-writer (:constructor %make-sst-file-writer (sap)))
  (sap nil :type (or null alien)))

(defun make-sst-file-writer (&optional comparator
                               (env-opts (rocksdb-envoptions-create))
                               (io-opts (rocksdb-options-create)))
  (%make-sst-file-writer
   (if comparator
       (create-sst-writer-with-comparator-raw comparator env-opts io-opts)
       (create-sst-writer-raw env-opts io-opts))))

(defun sst-file-size (writer)
  (declare (sst-file-writer writer))
  (sst-file-size-raw (sst-file-writer-sap writer)))

(defun open-sst (writer path)
  (declare (sst-file-writer writer))
  (open-sst-writer-raw (sst-file-writer-sap writer) path))

(defun finish-sst (writer)
  (declare (sst-file-writer writer))
  (finish-sst-writer-raw (sst-file-writer-sap writer)))

(defun destroy-sst (writer)
  (declare (sst-file-writer writer))
  (with-slots (sap) writer
    (unless (null sap)
      (destroy-sst-writer-raw sap)
      (setf sap nil))))

(defmethod print-object ((self sst-file-writer) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":size ~A" (when (sst-file-writer-sap self) (sst-file-size self)))))

(defmethod put-key ((self sst-file-writer) key val)
  (sst-put-raw (sst-file-writer-sap self) key val))

(defmethod put-key ((self sst-file-writer) (key simple-string) (val simple-string))
  (sst-put-str-raw (sst-file-writer-sap self) key val))

(defmethod put-kv ((self sst-file-writer) (kv rdb-kv))
  (sst-put-raw (sst-file-writer-sap self)
               (rdb-key kv) (rdb-val kv)))

(defmethod delete-key ((self sst-file-writer) key &key)
  (sst-delete-raw (sst-file-writer-sap self) key))

(defmethod delete-key-ts ((self sst-file-writer) key ts)
  (sst-delete-ts-raw (sst-file-writer-sap self) key ts))

(defmethod delete-key-range ((self sst-file-writer) start end &key)
  (sst-delete-range-raw (sst-file-writer-sap self) start end))

(defmethod put-key-ts ((self sst-file-writer) key val ts)
  (sst-put-ts-raw (sst-file-writer-sap self) key val ts))
