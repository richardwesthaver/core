;;; rdb/proto.lisp --- RDB protocol

;; Extends OBJ/DB protocol with RocksDB-specific generics.

;;; Code:
(in-package :rdb)

(defgeneric set-opt (self key val &key)
  (:documentation "Set value of option KEY to VAL."))
(defgeneric push-opts (self)
  (:documentation "Push all options to internal sap."))
(defgeneric backfill-opts (self &key)
  (:documentation "Backfill opts from an alien."))
(defgeneric push-sap (self key)
  (:documentation "Push a value associated with KEY to the sap associated
with SELF. Typically used to send a value from one slot, to a foreign
handle stored in another slot of the same object."))
(defgeneric push-sap* (self)
  (:documentation "Implicitly push values to the sap associated with SELF."))
(defgeneric pull-sap (self key)
  (:documentation "Pull a foreign value identified by KEY from the sap associated with SELF."))
(defgeneric pull-sap* (self)
  (:documentation "Implicitly pull foreign values from the sap associated with SELF."))
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
