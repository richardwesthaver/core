;;; log.lisp --- RDB Logger

;; RDB Logging Utilities

;;; Commentary:

;; Log messages to a dedicated logging db with RDB-SINK and RDB-LOGGER. The
;; RDB-LOG-SCHEMA class defines the required fields - one column-family per
;; log level.

;; For working with the logging mechanism within RocksDB itself, several
;; utility functions are provided with the RDB-LOG prefix.

;;; Code:
(in-package :rdb)

;;; RDB-LOG
(defun rdb-log-default (level &optional prefix)
  (if prefix
      (rocksdb-logger-create-stderr-logger level prefix)
      (rocksdb-logger-create-callback-logger 
       level 
       (alien-sap (alien-callable-function 'rocksdb-log-default))
       nil)))

;;; rdb-stats
(defstruct (rdb-stats (:constructor make-rdb-stats (&optional sap)))
  (sap nil :type (or null (alien (* rocksdb-statistics-histogram-data)))))

(defaccessor sap ((self rdb-stats)) (rdb-stats-sap self))

;;; metadata
(defstruct rdb-metadata
  (name "default" :type string)
  (size 0 :type fixnum)
  (level-count 7 :type fixnum)
  (file-count 0 :type fixnum)
  (sap nil :type (or null (alien (* rocksdb-column-family-metadata)))))

(defaccessor sap ((self rdb-metadata)) (rdb-metadata-sap self))
(defaccessor name ((self rdb-metadata)) (rdb-metadata-name self))

(defun print-stats (db &optional stream)
  (if stream
      (println (rocksdb-options-statistics-get-string (options db)) stream)
      (with-output-to-string (s)
        (print-stats db s))))

(defmethod metadata ((self rdb) &optional column)
  (make-rdb-metadata :sap (%get-metadata (db self) (when column (db column)))))

(defmethod db-stats ((self rdb) &optional (htype (rocksdb-statistics-level "all")))
  (%get-stats (options self) htype))

(defmethod metadata ((self rdb-metadata) &optional (level 0))
  (with-slots (sap) self
    (if (null sap)
        (warn 'metadata-missing :message "ignoring attempt to pull fields from null sap.")
        (make-level-metadata :sap (rocksdb-column-family-metadata-get-level-metadata sap level)))))

(defmethod print-object ((self rdb-metadata) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (name size level-count file-count) self
      (format stream "~A :size ~A :levels ~A :files ~A" name size level-count file-count))))

(defmethod alloc ((self rdb-metadata))
  (with-slots (name size level-count file-count sap) self
    (if (null sap)
        (warn 'metadata-missing :message "ignoring attempt to pull fields from null sap.")
        (setf name (rocksdb-column-family-metadata-get-name sap)
              size (rocksdb-column-family-metadata-get-size sap)
              level-count (rocksdb-column-family-metadata-get-level-count sap)
              file-count (rocksdb-column-family-metadata-get-file-count sap)))
    self))

(defstruct level-metadata
  (level 0 :type fixnum)
  (size 0 :type fixnum)
  (file-count 0 :type fixnum)
  (sap nil :type (or null (alien (* rocksdb-level-metadata)))))

(defaccessor sap ((self level-metadata)) (level-metadata-sap self))

(defmethod metadata ((self level-metadata) &optional (file 0))
  (if (null (sap self))
      (warn 'metadata-missing :message "ignoring attempt to pull fields from null sap.")
      (make-sst-file-metadata :sap (rocksdb-level-metadata-get-sst-file-metadata (sap self) file))))

(defmethod print-object ((self level-metadata) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (level size file-count) self
      (format stream "~A :size ~A :files ~A" level size file-count))))

(defmethod alloc ((self level-metadata))
  (with-slots (level size file-count sap) self
    (if (null sap)
        (warn 'metadata-missing :message "ignoring attempt to pull fields from null sap.")
        (setf level (rocksdb-level-metadata-get-level sap)
              size (rocksdb-level-metadata-get-size sap)
              file-count (rocksdb-level-metadata-get-file-count sap)))
    self))

;; NOTE: we only store the sizes of largest and smallest key, not the
;; keys themselves. This may change in the future.
(defstruct sst-file-metadata
  (relative-filename "" :type string)
  (directory "" :type string)
  (size 0 :type fixnum)
  (smallestkey 0 :type fixnum)
  (largestkey 0 :type fixnum)
  (sap nil :type (or null (alien (* rocksdb-sst-file-metadata)))))

(defaccessor sap ((self sst-file-metadata)) (sst-file-metadata-sap self))

(defmethod print-object ((self sst-file-metadata) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (relative-filename directory size smallestkey largestkey) self
      (format stream "~A :dir ~A :size ~A :smallest ~A :largest ~A"
              relative-filename directory size smallestkey largestkey))))

(defmethod alloc ((self sst-file-metadata))
  (with-slots (relative-filename directory size smallestkey largestkey sap) self
    (if (null sap)
        (warn 'metadata-missing :message "ignoring attempt to pull fields from null sap.")
        (with-alien ((ssize size-t 0)
                     (lsize size-t 0))
          (rocksdb-sst-file-metadata-get-largestkey sap (addr lsize))
          (rocksdb-sst-file-metadata-get-smallestkey sap (addr ssize))
          (setf relative-filename (rocksdb-sst-file-metadata-get-relative-filename sap)
                directory (rocksdb-sst-file-metadata-get-directory sap)
                size (rocksdb-sst-file-metadata-get-size sap)
                largestkey lsize
                smallestkey ssize)))
    self))

;;; Logger
(defclass rdb-log-schema (rdb-schema) ()
  (:default-initargs
   :fields 
   (make-fields 
    ;; log levels
    :trace '(octet-vector . string)
    :debug '(octet-vector . string)
    :info '(octet-vector . string)
    :warn '(octet-vector . string)
    :error '(octet-vector . string))))

(defclass rdb-sink (db-sink rdb) ()
  (:default-initargs
   :db (make-db :rdb)))

;; TODO 2025-05-30: 
(defclass rdb-logger (database-logger) ()
  (:documentation "A LOGGER with a SINK that writes LOG-MESSAGEs to an RDB."))

