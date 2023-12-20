(in-package :rocksdb)

(define-alien-routine rocksdb-sstfilewriter-create (* rocksdb-sstfilewriter)
  (env (* rocksdb-envoptions))
  (io-options (* rocksdb-options)))

(define-alien-routine rocksdb-sstfilewriter-create-with-comparator (* rocksdb-sstfilewriter)
  (env (* rocksdb-envoptions))
  (io-options (* rocksdb-options))
  (comparator (* rocksdb-comparator)))

(define-alien-routine rocksdb-sstfilewriter-destroy void (writer (* rocksdb-sstfilewriter)))

(export '(rocksdb-sstfilewriter-create rocksdb-sstfilewriter-create-with-comparator
          rocksdb-sstfilewriter-destroy))

(def-with-errptr rocksdb-sstfilewriter-open void
  (writer (* rocksdb-sstfilewriter))
  (name c-string))

(def-with-errptr rocksdb-sstfilewriter-add void
  (writer (* rocksdb-sstfilewriter))
  (key (* char))
  (keylen size-t)
  (val (* char))
  (vallen size-t))

(def-with-errptr rocksdb-sstfilewriter-put void
  (writer (* rocksdb-sstfilewriter))
  (key (* char))
  (keylen size-t)
  (val (* char))
  (vallen size-t))

(def-with-errptr rocksdb-sstfilewriter-put-with-ts void
  (writer (* rocksdb-sstfilewriter))
  (key (* char))
  (keylen size-t)
  (ts (* char))
  (tslen size-t)
  (val (* char))
  (vallen size-t))

(def-with-errptr rocksdb-sstfilewriter-merge void
  (writer (* rocksdb-sstfilewriter))
  (key (* char))
  (keylen size-t)
  (val (* char))
  (vallen size-t))
  
(def-with-errptr rocksdb-sstfilewriter-delete void
  (writer (* rocksdb-sstfilewriter))
  (key (* char))
  (keylen size-t))

(def-with-errptr rocksdb-sstfilewriter-delete-with-ts void
  (writer (* rocksdb-sstfilewriter))
  (key (* char))
  (keylen size-t)
  (ts (* char))
  (tslen size-t))

(def-with-errptr rocksdb-sstfilewriter-delete-range void
  (writer (* rocksdb-sstfilewriter))
  (begin-key (* char))
  (begin-keylen size-t)
  (end-key (* char))
  (end-keylen size-t))

(def-with-errptr rocksdb-sstfilewriter-finish void
  (writer (* rocksdb-sstfilewriter)))

(def-with-errptr rocksdb-sstfilewriter-file-size void
  (writer (* rocksdb-sstfilewriter))
  (file-size (* unsigned-long)))
