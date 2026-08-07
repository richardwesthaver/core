;;; rdb/proto.lisp --- RDB protocol

;; Extends OBJ and STD protocols with RocksDB-specific additions.

;;; Code:
(in-package :rdb)
(defgeneric print-stats (self &optional stream)
  (:documentation "Print statistics data from SELF."))
(defgeneric open-backup-engine (self &key path))
(defgeneric close-backup-engine (self))
(defgeneric open-backup-db (self &key path))
(defgeneric open-checkpoint-db (self &key path))
(defgeneric close-checkpoint-db (self &key path))
(defgeneric close-backup-db (self))
(defgeneric open-transaction-db (self &key path opts))
(defgeneric close-transaction-db (self))
(defgeneric open-secondary-db (self &key opts path))
(defgeneric close-secondary-db (self))
(defgeneric read-opts (self))
(defgeneric (setf read-opts) (new self))
(defgeneric write-opts (self))
(defgeneric (setf write-opts) (new self))
