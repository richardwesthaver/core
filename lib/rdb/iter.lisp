;;; iter.lisp --- Iterators

;; Implementation of ITER and CURSOR protocols based on the ROCKSDB-ITER type.

;;; Code:
(in-package :rdb)
;;; Iterator
(defmethod seek-to-first ((self rdb-iter))
  (rocksdb-iter-seek-to-first (rdb-iter-sap self)))

(defmethod seek-to-last ((self rdb-iter))
  (rocksdb-iter-seek-to-last (rdb-iter-sap self)))

(defmethod seek-for-prev ((self rdb-iter) (key vector) &key)
  (rocksdb-iter-seek-for-prev (rdb-iter-sap self) (octets-to-alien key) (length key)))

(defmethod seek ((self rdb-iter) (key simple-vector) &key)
  (rocksdb-iter-seek (rdb-iter-sap self) (octets-to-alien key) (length key)))

(defmethod next ((self rdb-iter))
  (rocksdb-iter-next (rdb-iter-sap self)))

(defmethod prev ((self rdb-iter))
  (rocksdb-iter-prev (rdb-iter-sap self)))

(defmethod key ((self rdb-iter))
  (with-alien ((klen size-t))
    (let ((key (rocksdb-iter-key (rdb-iter-sap self) (addr klen))))
      (let ((k (make-octets klen)))
        (clone-octets-from-alien key k)
        (values
         k
         klen)))))

(defmethod val ((self rdb-iter))
  (with-alien ((vlen size-t))     
    (let ((val (rocksdb-iter-value (sap self) (addr vlen))))
      (let ((v (make-octets vlen)))
        (clone-octets-from-alien val v)
        (values
         v
         vlen)))))

(defmethod timestamp ((self rdb-iter))
  (with-alien ((tslen size-t))
    (values
     (rocksdb-iter-timestamp (sap self) (addr tslen))
     tslen)))
