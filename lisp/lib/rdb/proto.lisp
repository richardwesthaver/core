;;; rdb/proto.lisp --- RDB protocol

;; Extends OBJ/DB protocol with RocksDB-specific generics.

;;; Code:
(in-package :rdb)

;; Most methods are intended to work with the RDB struct directly
(defgeneric put-kv (self kv)
  (:documentation "Insert a KeyVal object."))
(defgeneric put-key (self key val)
  (:documentation "Insert a KEY and VAL."))
(defgeneric get-key (self key)
  (:documentation "Get value of KEY."))
(defgeneric get-opt (self key)
  (:documentation "Get value of option KEY."))
(defgeneric set-opt (self key val &key &allow-other-keys)
  (:documentation "Set value of option KEY to VAL."))
(defgeneric push-sap (self key))
(defgeneric push-sap* (self))
(defgeneric push-cf (self cf))
(defgeneric insert-key (self key val &key &allow-other-keys))
(defgeneric insert-kv (self kv &key &allow-other-keys))
