(in-package :rdb)

;; Most methods are intended to work with the RDB struct
(defgeneric put-kv (self key val &key &allow-other-keys))
(defgeneric get-kv (self key &key &allow-other-keys))
