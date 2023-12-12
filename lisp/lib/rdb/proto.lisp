(in-package :rdb)

;; Most methods are intended to work with the RDB struct directly
(defgeneric put-kv (self kv))
(defgeneric put-key (self key val))
(defgeneric get-key (self key))

(defgeneric get-opt (self key))
(defgeneric set-opt (self key val &key &allow-other-keys))
(defgeneric push-sap (self key))
(defgeneric push-sap* (self))

(defgeneric push-cf (self cf))

(defgeneric make-db (self &key &allow-other-keys))
(defgeneric open-db (self))
(defgeneric close-db (self))
(defgeneric destroy-db (self))
(defgeneric init-db (self))
(defgeneric insert-key (self key val &key &allow-other-keys))
(defgeneric insert-kv (self kv &key &allow-other-keys))
