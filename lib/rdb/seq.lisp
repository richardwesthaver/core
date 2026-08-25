;;; seq.lisp --- RocksDB sequences

;; persistent sequence numbers

;;; Code:
(in-package :rdb)

(defclass counter-column-family (column-family) 
  ((data :initarg :data :type (simple-array word (*)) :accessor data))
  (:documentation "A column-family which stores individual (unsigned-byte 64) values."))

(defmethod get-value ((elt number) (obj counter-column-family))
  (aref (slot-value obj 'data) elt))

(defmethod init ((self counter-column-family) &key (db *db*))
  "Initialize DATA from an open column-family."
  (let ((it (iter db :column self)))
    (iter-seek :first it)
    (with-buffer-streams (kbuf vbuf)
      (setf (slot-value self 'data)
            (coerce 
             (loop while (iter-seek :next it)
                   for i from 0
                   collect (multiple-value-bind (k v) (iter-get it kbuf vbuf)
                             (assert (= i (read-buffer-fixnum64 k)) nil 
                                     'db-error :message "invalid counter column.")
                             (prog1 (read-buffer-fixnum64 v)
                               (reset-buffer-stream kbuf)
                               (reset-buffer-stream vbuf))))
             '(simple-array word (*)))))
    (rocksdb-iter-destroy it)
    self))

(defmethod flush ((self counter-column-family) &key (db *db*))
  (loop for c across (the (simple-array word (*)) (slot-value self 'data))
        for i from 0
        do (with-buffer-streams (kbuf vbuf)
             (write-buffer-fixnum64 i kbuf)
             (write-buffer-fixnum64 c vbuf)
             (db-put db kbuf vbuf :cf (db self)))))

(defmethod save ((self counter-column-family) &key (db *db*))
  (flush self :db db)
  self)
