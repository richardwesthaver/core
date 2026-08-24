;;; seq.lisp --- RocksDB sequences

;; persistent sequence numbers

;;; Code:
(in-package :rdb)

(defclass counter-column-family (column-family) 
  ((counts :initarg :counts :type (simple-array word (*))))
  (:documentation "A column-family which stores individual (unsigned-byte 64) values."))

(defmethod get-value ((elt number) (obj counter-column-family))
  (aref (slot-value obj 'counts) elt))

(defmethod flush ((self counter-column-family) &key (db *db*))
  (loop for c across (the (simple-array word (*)) (slot-value self 'counts))
        for i from 0
        do (with-buffer-streams (kbuf vbuf)
             (write-buffer-fixnum64 i kbuf)
             (write-buffer-fixnum64 c vbuf)
             (db-put (db db) kbuf vbuf :cf (db self)))))
