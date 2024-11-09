;;; rdb/proto.lisp --- RDB protocol

;; Extends OBJ and STD protocols with RocksDB-specific additions.

;;; Code:
(in-package :rdb)
(defgeneric push-opts (self)
  (:documentation "Push all options to internal sap."))
(defgeneric backfill-opts (self &key)
  (:documentation "Backfill opts from an alien."))
(defgeneric print-stats (self &optional stream)
  (:documentation "Print statistics data from SELF."))
(defgeneric create-iter (self &optional cf opts)
  (:documentation "Create an interator over the kvs of SELF module CF and OPTS."))
(defgeneric iter-next (self)
  (:documentation "Return the next value."))
(defgeneric iter-prev (self)
  (:documentation "Return the previous value."))
(defgeneric iter-seek (self key &key)
  (:documentation "Seek to a certain KEY in the iterator."))
(defgeneric iter-val (self)
  (:documentation "Return the value of current iterator item."))
(defgeneric iter-valid-p (self)
  (:documentation "Return non-nil if the iterator cursor is valid."))
(defgeneric iter-key (self)
  (:documentation "Return the key of current iterator item."))
(defgeneric iter-kv (self)
  (:documentation "Return the current KV object of the iterator by getting the key and
val."))
(defgeneric iter-timestamp (self)
  (:documentation "Return the timestamp of current iterator item."))
