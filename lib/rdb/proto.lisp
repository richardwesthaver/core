;;; rdb/proto.lisp --- RDB protocol

;; Extends OBJ and STD protocols with RocksDB-specific additions.

;;; Code:
(in-package :rdb)
(defgeneric backup (self &key path))
(defgeneric backup-db (self &key path))
(defgeneric close-backup (self))
(defgeneric checkpoint (self &key path))
(defgeneric close-checkpoint (self))
(defgeneric open-secondary-db (self &key opts path))
(defgeneric close-secondary-db (self))
