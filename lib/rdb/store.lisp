;;; store.lisp --- RocksDB Store

;; OBJ/STORE implementation for RocksDB

;;; Commentary:

;; based on Elephant which is generic but default implementation is BDB - in
;; our case we use RocksDB (RDB). The semantics are somewhat different.

;; For starters BDB uses the term 'cursor' but RocksDB has 'iterators' which
;; themselves contain an internal position and bookkeeping. Don't forget about
;; the iterator refresh feature, which will free resources blocked by the
;; iterator - something we are likely to need to do often.

;; RocksDB is much more advanced than BDB - there are features which are
;; likely to come in handy such as Multi-CF Iterators, but may need time to
;; find where they can be used.

;; The 'default' column-family is the ROOT of the store btree - each CF is an
;; individual RDB-BTREE. the implementation details are otherwise hidden from
;; the database and handled in Lisp.

;; The sequence number, which is an arbitrarily increasing integer is used as
;; a state indicator on opening a store and is pulled from the RocksDB
;; `rocksdb::rocksdb-livefiles-largest-seqno'. 

;; Naturally to support Elephant's transaction abstraction we leverage a
;; TransactionDB and generate transactions via ffi as needed. The transaction
;; protocol is not specific to stores though - be careful of overlapping with
;; a plain DB transaction.

;;; Code:
(in-package :rdb)
(defconstant +reserved-dbinfo+ #xF0)
(defconstant +store-major-version+ 0)
(defconstant +store-minor-version+ 1)
(defvar *store-version* (list +store-major-version+ +store-minor-version+ 0))

(defun serialize-reserved-tag (bs)
  (write-buffer-byte +reserved-dbinfo+ bs))
(defun serialize-system-tag (byte bs)
  (write-buffer-byte byte bs))

(defun serialize-system-integer (int bs)
  (write-buffer-int32 int bs))
(defun deserialize-system-integer (bs)
  (read-buffer-int32 bs))

;; Database Version (a list of integers = [version major minor])
(defun serialize-database-version-key (bs)
  "Given a buffer-stream, encode a key indicating the version using
the constant +store-major-version+"
  (serialize-reserved-tag bs)
  (serialize-system-tag +store-major-version+ bs))

(defun serialize-database-version-value (version bs)
  "Serializes a list containing three integers to the buffer stream bs"
  (assert (= (length version) 3))
  (destructuring-bind (version major minor) version
    (serialize-system-integer version bs)
    (serialize-system-integer major bs)
    (serialize-system-integer minor bs)))

(defun deserialize-database-version-value (bs)
  "Deserializes the 3 integer list from buffer stream bs"
  (let ((version (deserialize-system-integer bs))
    (major (deserialize-system-integer bs))
    (minor (deserialize-system-integer bs)))
    (list version major minor)))

(defun serialize-database-serializer-version-key (bs)
  (serialize-reserved-tag bs)
  (serialize-system-tag +store-minor-version+ bs))

(defun serialize-database-serializer-version-value (version bs)
  (serialize-system-integer version bs))

(defun deserialize-database-serializer-version-value (bs)
  (deserialize-system-integer bs))

;;; Store Spec
;; (:rdb "/tmp/rdb-store-test")
(defun rdb-store-spec-p (spec)
  (and (consp spec)
       (eq (first spec) :rdb)
       (typecase (second spec)
         (pathname t)
         (string t)
         (t nil))))

(defun rdb-temp-spec (name)
  `(:rdb ,(tmp-path name) 
         ,(default-rocksdb-options)
         ,(default-rocksdb-transactiondb-options)))

;;; BTrees
(defclass rdb-btree (btree) ()
  (:documentation "A RocksDB implementation of a BTree."))

;;; Store
(defclass rdb-store (store trdb)
  ((seqs :accessor seqs)
   ;; FIX 2026-07-28: we should use something like 'mix' when we need a logger
   ;; (logger :initform (default-logger) :initarg :logger :accessor logger)
   (metadata :accessor store-metadata)
   (oids :accessor oids)
   (btrees :accessor btrees)
   (dup-btrees :accessor dup-btrees)
   (index :accessor index)
   (rindex :accessor rindex))
  (:default-initargs
   :columns nil
   :options nil
   :transactiondb-options nil
   :path nil
   :de #'deserialize-object
   :ser #'serialize-object)
   ;; (make-instance 'simple-column-family :type '(oid . cid) :name "instance-index")
   ;; (make-instance 'simple-column-family :type '(cid . oid) :name "class-index")
   ;; :root
  (:documentation "A RocksDB-based STORE."))

(defmethod make-btree ((st rdb-store))
  (make-instance 'rdb-btree :store st))

;; (make-btree (make-instance 'rdb-store))
(defaccessor options ((self rdb-store)) (caddr (spec self)))
(defaccessor transactiondb-options ((self rdb-store)) (cadddr (spec self)))
(defaccessor path ((self rdb-store)) (cadr (spec self)))
(defmethod options :around ((self rdb-store))
  (or (slot-value self 'options) (call-next-method self)))
(defmethod transactiondb-options :around ((self rdb-store))
  (or (slot-value self 'transactiondb-options) (call-next-method self)))
(defmethod path :around ((self rdb-store))
  (or (slot-value self 'path) (call-next-method self)))

(defmethod version ((self rdb-store))
  (with-buffer-streams (key)
    (serialize-database-version-key key)
    (let ((buf (trdb-get (db self) key :cf (db (store-metadata self)))))
      (when buf (deserialize-database-version-value buf)))))

(defun set-database-version (sc cf)
  "Internal use when creating new database"
  (with-buffer-streams (key val)
    (serialize-database-version-key key)
    (serialize-database-version-value *store-version* val)
    (trdb-put (db sc) key val :cf cf)
    *store-version*))

;;; Interface
;; the following methods up to the open/close section use BUFFER-STREAMs,
;; Transactions, and the default store serde - SERIALIZE-OBJECT and
;; DESERIALIZE-OBJECT.
(defmethod get-value (key (bt rdb-btree))
  "Getting a value from a plain RDB-BTREE will fetch the value directly from (DB
*STORE*)."
  (trace! "get-value" key bt)
  (let ((sc (get-store bt)))
    (ensure-transaction (:db sc)
      (with-buffer-streams (key-buf)
        (write-buffer-oid (oid bt) key-buf)
        (serialize-object key key-buf sc)
        (let ((buf (txn-get
                    key-buf
                    :cf (db (btree sc)))))
          (if buf 
              (values (deserialize-object buf sc) t)
              (values nil nil)))))))

(defmethod existsp (key (bt rdb-btree))
  (let ((sc (get-store bt)))
    (ensure-transaction (:db sc)
      (with-buffer-streams (key-buf)
        (write-buffer-oid (oid bt) key-buf)
        (serialize-object key key-buf sc)
        (let ((buf (txn-get
                    key-buf 
                    :cf (db (btree sc)))))
          (if buf t nil))))))

(defmethod (setf get-value) (value key (bt rdb-btree))
  (let ((sc (get-store bt)))
    (ensure-transaction (:db sc)
      (with-buffer-streams (key-buf value-buf)
        (write-buffer-oid (oid bt) key-buf)
        (serialize-object key key-buf sc)
        (serialize-object value value-buf sc)
        (txn-insert (get-store (btree sc))
                    key-buf value-buf
                    :cf (db (btree sc)))))
    value))

(defmethod delete-key (key (bt rdb-btree) &key)
  (let ((sc (get-store bt)))
    (with-buffer-streams (key-buf)
      (ensure-transaction (:db sc)
        (write-buffer-oid (oid bt) key-buf)
        (serialize-object key key-buf sc)
        (txn-delete key-buf :cf (btree sc))))))

(defmethod optimize-layout ((bt rdb-btree) &key (freelist-only t) (free-space nil) &allow-other-keys)
  (optimize-layout (get-store bt)
                   :start-key (oid bt)
                   :end-key (oid bt)
                   :freelist-only freelist-only
                   :free-space free-space))

(defsclass rdb-indexed-btree (indexed-btree rdb-btree)
  ((index :accessor index :initarg :index :initform (make-hash-table))
   (index-cache :accessor index-cache :transient t))
  (:documentation "A RDB-based BTree supports secondary index-table."))

;; TODO 2026-08-19: memoize
(defmethod index-cache ((instance rdb-indexed-btree))
  ;; Lazily load the index-cache to avoid bootstrapping issues: If
  ;; we do not lazy-load the index-table cache, it we attempt to
  ;; initialize it before the instance-table is available (thus we
  ;; cannot map oids to classes -- deserialize does not really work
  ;; for complex objects).  -- Red Daly 07/10/2010
  (ifret (slot-boundp! instance 'index-cache)
    (setf (index-cache instance) (index instance))))

(defmethod shared-initialize :after ((instance rdb-indexed-btree) slot-names
                                     &rest rest)
  (declare (ignore slot-names rest))
  (setf (index-cache instance) nil))

(defmethod make-indexed-btree ((sc rdb-store))
  (make-instance 'rdb-indexed-btree :store sc))

(defmethod make-btree-index ((sc rdb-store) &key primary key-form &allow-other-keys)
  (make-instance 'rdb-btree-index :primary primary :key-form key-form :store sc))

(defmethod add-index ((bt rdb-indexed-btree) &key index-name key-form (populate t))
  (let ((sc (get-store bt)))
    ;; FIX 2026-08-20: 
    ;; Setting the value of *store* is unfortunately absolutely required at
    ;; present, I think because the copying of objects is calling
    ;; "make-instance" without an argument.
    (if (and (not (null index-name))
             (symbolp index-name)
             (or (symbolp key-form) (listp key-form)))
        ;; could it be that this fails?
        (let ((index
                (ensure-transaction (:db sc)
                  (let ((ht (index bt))
                        (index (make-btree-index sc 
                                                 :primary bt 
                                                 :key-form key-form)))
                    (setf (gethash index-name (index-cache bt)) index)
                    (setf (gethash index-name ht) index)
                    (setf (index bt) ht)
                    index))))
          (when populate (populate bt index))
          index)
        (error "Invalid index initargs!"))))

(defmethod populate ((bt rdb-indexed-btree) index)
  (trace! "populating indexed-btree" bt index)
  (let ((sc (get-store bt)))
    (with-buffer-streams (primary-buf secondary-buf)
      (flet ((.index (key skey)
               (write-buffer-oid (oid bt) primary-buf)
               (serialize-object key primary-buf sc)
               (write-buffer-oid (oid index) secondary-buf)
               (serialize-object skey secondary-buf sc)
               ;; should silently do nothing if the key/value already exists
               (txn-insert sc secondary-buf primary-buf :cf (db (index sc)))
               (free primary-buf)
               (free secondary-buf)))
        (let ((key-fn (key-fn index))
              (last-key nil)
              (continue t))
          (loop while continue
                do
                   (ensure-transaction (:db sc)
                     (with-btree-cursor (cursor bt)
                       (if last-key 
                           (cursor-set cursor last-key)
                           (cursor-first cursor))
                       (loop for i from 0 upto 1000
                             while continue
                             do
                                (multiple-value-bind (valid? k v) (cursor-current cursor)
                                  (unless valid? (return-from populate t))
                                  (multiple-value-bind (index? skey) (funcall key-fn index k v)
                                    (when index? (.index k skey))))
                                (multiple-value-bind (valid? k v) (cursor-next cursor)
                                  (declare (ignore v))
                                  (if valid? 
                                      (setf last-key k)
                                      (setf continue nil))))))))))))


(defmethod map-index (fn (bt rdb-indexed-btree) &key)
  (maphash fn (index-cache bt)))

(defmethod get-index ((bt rdb-indexed-btree) index-name)
  (gethash index-name (index-cache bt)))

(defmethod remove-index ((bt rdb-indexed-btree) index-name)
  (remhash index-name (index-cache bt))
  (let ((index (index bt)))
    (remhash index-name index)
    (setf (index bt) index)))

(defmethod (setf get-value) (value key (bt rdb-indexed-btree))
  "Set a key / value pair, and update secondary index."
  (trace! "setf GET-VALUE" value key bt)
  (let ((sc (get-store bt)))
    (let ((index (index-cache bt)))
      (with-buffer-streams (key-buf value-buf secondary-buf)
        (write-buffer-oid (oid bt) key-buf)
        (serialize-object key key-buf sc)
        (serialize-object value value-buf sc)
        (ensure-transaction (:db sc)
          (txn-insert sc key-buf value-buf :cf (db (btree sc)))
          ;; Manually write value into secondary index
          (loop for idx being the hash-value of index
                do
                   (multiple-value-bind (index? secondary-key)
                       (funcall (key-fn idx) idx key value)
                     (when index?
                       ;; Insert
                       (write-buffer-oid (oid idx) secondary-buf)
                       (serialize-object secondary-key secondary-buf sc)
                       (txn-insert sc 
                                   secondary-buf key-buf
                                   :cf (db (index sc))
                                   ;;  :no-dup t
                                   )
                       (reset-buffer-stream secondary-buf))))
          value)))))

(defmethod delete-key (key (bt rdb-indexed-btree) &key)
  "Remove a key / value pair, and update secondary index-table."
  (let ((sc (get-store bt)))
    (with-buffer-streams (key-buf secondary-buf)
      (write-buffer-oid (oid bt) key-buf)
      (serialize-object key key-buf sc)
      (ensure-transaction (:db sc)
        (let ((value (get-value key bt)))
          (when value
            (let ((index-table (index-cache bt)))
              (loop 
                for index being the hash-value of index-table
                do (multiple-value-bind (index? secondary-key)
                       (funcall (key-fn index) index key value)
                     (when index?
                       (write-buffer-oid (oid index) secondary-buf)
                       (serialize-object secondary-key secondary-buf sc)
                       (txn-delete
                        secondary-buf
                        :cf (db (index (get-store bt))))
                       (reset-buffer-stream secondary-buf))))
              (txn-delete key-buf
                          :cf (db (btree (get-store bt)))))))))))

;; This also needs to build the correct kind of index, and 
;; be the correct kind of btree...
(defsclass rdb-btree-index (btree-index rdb-btree)
  ()
  (:documentation "A RDB-based BTree supporting secondary index tables."))

(defmethod get-value (key (bt rdb-btree-index))
  "Get the value in the primary DB from a secondary key."
  (let ((sc (get-store bt)))
    (with-buffer-streams (key-buf)
      (write-buffer-oid (oid bt) key-buf)
      (serialize-object key key-buf sc)
      (let ((buf (txn-get key-buf :cf (db (rindex sc)))))
        (if buf (values (deserialize-object buf sc) t)
            (values nil nil))))))

(defmethod get-primary-key (key (bt btree-index))
  (let ((sc (get-store bt)))
    (with-buffer-streams (key-buf)
      (write-buffer-oid (oid bt) key-buf)
      (serialize-object key key-buf sc)
      (let ((buf (txn-get key-buf :cf (db (index sc)))))
        (if buf 
            (let ((oid (read-buffer-oid buf)))
              (values (deserialize-object buf sc) oid))
            (values nil nil))))))

(defclass rdb-cursor (cursor)
  ((sap :accessor sap :initarg :sap))
  (:documentation "A cursor for traversing (primary) RDB-BTrees.
The SAP slot contains a pointer to the underlying ROCKSDB-ITERATOR."))

(defmethod make-cursor ((bt rdb-btree))
  "Make a cursor from a btree."
  (let ((sc (get-store bt)))
    (make-instance 'rdb-cursor 
      :btree bt
      ;;  HACK 2026-08-04: should this be (iter (or txn (db sc)) :column (btree sc))?
      :sap (iter (or (and *transaction* (txn-iter))  (btree sc)))
      :oid (oid bt))))

(defmethod cursor-close ((cursor rdb-cursor))
  (when (sap cursor)
    (setf (sap cursor) (rocksdb-iter-destroy (sap cursor))))
  (setf (cursor-initialized-p cursor) nil))

(defmethod cursor-duplicate ((cursor rdb-cursor))
  (make-instance (type-of cursor)
    :initialized-p (cursor-initialized-p cursor)
    :oid (cursor-oid cursor)
    :sap (iter
          (get-store (btree cursor))
          :column (btree cursor)
          :position (cursor-initialized-p cursor))))

(defmethod cursor-current ((cursor rdb-cursor))
  (when (cursor-initialized-p cursor)
    (let ((sc (get-store (btree cursor))))
      (with-buffer-streams (key-buf value-buf)
        (multiple-value-bind (key val) (iter-get (sap cursor) key-buf value-buf)
          (if (and key (= (read-buffer-oid key) (cursor-oid cursor)))
              (progn (setf (cursor-initialized-p cursor) t)
                     (values t (deserialize-object key sc)
                             (deserialize-object val sc)))
              (setf (cursor-initialized-p cursor) nil)))))))

(defmethod cursor-first ((cursor rdb-cursor))
  (let ((sc (get-store (btree cursor))))
    (with-buffer-streams (key-buf value-buf)
      (write-buffer-oid (cursor-oid cursor) key-buf)
      (multiple-value-bind (key val) (iter-move :first (sap cursor) key-buf value-buf) ; :set-range t
        (if (and key (= (read-buffer-oid key) (cursor-oid cursor)))
            (progn (setf (cursor-initialized-p cursor) t)
                   (values t 
                           (deserialize-object key sc)
                           (deserialize-object val sc)))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-last ((cursor rdb-cursor))
  "A fast cursor last, but a bit 'hackish' by exploiting oid ordering."
  (let ((sc (get-store (btree cursor))))
    (with-buffer-streams (key-buf value-buf)
      ;; FIX 2026-08-23: could just use seek-to-last..
      ;; Go to the first element of the next btree
      (write-buffer-oid (+ (cursor-oid cursor) 1) key-buf)
      (if (iter-move :seek (sap cursor) key-buf value-buf) ; :set-range t
          (progn (reset-buffer-stream key-buf)
                 (reset-buffer-stream value-buf)
                 ;; move the iterator back by 1
                 (multiple-value-bind (key val) (iter-move :prev (sap cursor) key-buf value-buf)
                   (if (and key (= (read-buffer-oid key) (cursor-oid cursor)))
                       (progn
                         (setf (cursor-initialized-p cursor) t)
                         (values t (deserialize-object key sc)
                                 (deserialize-object val sc)))
                       (setf (cursor-initialized-p cursor) nil))))
          (multiple-value-bind (key val) (iter-move :last (sap cursor) key-buf value-buf)
            (if (and key (= (read-buffer-oid key) (cursor-oid cursor)))
                (progn
                  (setf (cursor-initialized-p cursor) t)
                  (values t (deserialize-object key sc)
                          (deserialize-object val sc )))
                (setf (cursor-initialized-p cursor) nil)))))))

(defmethod cursor-next ((cursor rdb-cursor))
  (if (cursor-initialized-p cursor)
      (let ((sc (get-store (btree cursor))))
        (with-buffer-streams (key-buf value-buf)
          (multiple-value-bind (key val)
              (the (values (or null buffer-stream)
                           (or null buffer-stream))
                   (iter-move :next (sap cursor) key-buf value-buf))
            (if (and key (= (read-buffer-oid key) (cursor-oid cursor)))
                (the (values t t t)
                     (values t (deserialize-object key sc)
                             (deserialize-object val sc)))
                (the null (setf (cursor-initialized-p cursor) nil))))))
      (the t (cursor-first cursor))))

(defmethod cursor-prev ((cursor rdb-cursor))
  (if (cursor-initialized-p cursor)
      (let ((sc (get-store (btree cursor))))
        (with-buffer-streams (key-buf value-buf)
          (multiple-value-bind (key val) (iter-move :prev (sap cursor) key-buf value-buf)
            (if (and key (= (read-buffer-oid key) (cursor-oid cursor)))
                (values t (deserialize-object key sc)
                        (deserialize-object val sc))
                (setf (cursor-initialized-p cursor) nil)))))
      (cursor-last cursor)))

(defmethod cursor-set ((cursor rdb-cursor) key)
  (let ((sc (get-store (btree cursor))))
    (with-buffer-streams (key-buf value-buf)
      (write-buffer-oid (cursor-oid cursor) key-buf)
      (serialize-object key key-buf sc)
      (multiple-value-bind (k val)
          (txn-iter-set (sap cursor)
                        key-buf value-buf 
                        ;; :set t
                        )
        (if k
            (progn
              (setf (cursor-initialized-p cursor) t)
              (values t key (deserialize-object val sc)))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-set-range ((cursor rdb-cursor) key)
  (let ((sc (get-store (btree cursor))))
    (with-buffer-streams (key-buf value-buf)
      (write-buffer-oid (cursor-oid cursor) key-buf)
      (serialize-object key key-buf sc)
      (multiple-value-bind (k val) (txn-iter-set (sap cursor) key-buf value-buf) ; :set-range t
        (if (and k (= (read-buffer-oid k) (cursor-oid cursor)))
            (progn (setf (cursor-initialized-p cursor) t)
                   (values t (deserialize-object k sc)
                           (deserialize-object val sc)))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-get-both ((cursor rdb-cursor) key value)
  (let ((sc (get-store (btree cursor))))
    (with-buffer-streams (key-buf value-buf)
      (write-buffer-oid (cursor-oid cursor) key-buf)
      (serialize-object key key-buf sc)
      (serialize-object value value-buf sc)
      (multiple-value-bind (k v) (iter-move :seek (sap cursor) key-buf value-buf) ; :get-both t
        (declare (ignore v))
        (if k
            (progn (setf (cursor-initialized-p cursor) t)
                   (values t key value))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-get-both-range ((cursor rdb-cursor) key value)
  (let ((sc (get-store (btree cursor))))
    (with-buffer-streams (key-buf value-buf)
      (write-buffer-oid (cursor-oid cursor) key-buf)
      (serialize-object key key-buf sc)
      (serialize-object value value-buf sc)
      (multiple-value-bind (k v) (iter-get (sap cursor) key-buf value-buf) ; :get-both-range t
        (if k (progn (setf (cursor-initialized-p cursor) t)
                     (values t key (deserialize-object v sc)))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-delete ((cursor rdb-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf value-buf)
        (multiple-value-bind (key val) (iter-get (sap cursor) key-buf value-buf) ; :current t?
          (declare (ignore val))
          (when (and key (= (read-buffer-oid key) (cursor-oid cursor)))
            ;; in case of a secondary index this should delete everything
            ;; as specified by the RDB docs.
            (delete-key (deserialize-object key (get-store (btree cursor)))
                        (btree cursor)))
          (setf (cursor-initialized-p cursor) nil)))
      (error "Can't delete with uninitialized cursor!")))

(defmethod cursor-put ((cursor rdb-cursor) value &key (key nil key-specified-p))
  "Put by cursor.  Not particularly useful since standard btrees
   don't support duplicates.  Cursor is invalid after a put"
  (if key-specified-p
      (setf (get-value key (btree cursor)) value)
      (if (cursor-initialized-p cursor)
          (let ((sc (get-store (btree cursor))))
            (with-buffer-streams (key-buf value-buf)
              (multiple-value-bind (k v) (iter-get (sap cursor) key-buf value-buf)
                (declare (ignore v))
                (if (and k (= (read-buffer-oid k) (cursor-oid cursor)))
                    (progn
                      (setf (get-value (deserialize-object k sc) (btree cursor))
                            value)
                      (reset-buffer-stream key-buf) (reset-buffer-stream value-buf)
                      (multiple-value-bind (k v) (iter-move :next (sap cursor) key-buf value-buf)
                        (if (and key (= (read-buffer-oid k) (cursor-oid cursor)))
                            (values t (deserialize-object k sc) (deserialize-object v sc))
                            (setf (cursor-initialized-p cursor) nil))))
                    (setf (cursor-initialized-p cursor) nil)))))
          (error "Can't put with uninitialized cursor!"))))

;; Secondary cursors
(defclass rdb-secondary-cursor (secondary-cursor rdb-cursor) 
  ((primary :initarg :primary :reader primary
            :documentation "The primary column-family associated with this cursor."))
  (:documentation "Cursor for traversing rdb secondary index-table."))

(defmethod make-cursor ((bt rdb-btree-index))
  "Make a secondary-cursor from a secondary index."
  (let ((sc (get-store bt)))
    (make-instance 'rdb-secondary-cursor 
      :btree bt
      :sap (or (and *transaction* (txn-iter)) (db (rindex sc)))
      :oid (oid bt))))

(defmethod cursor-pcurrent ((cursor rdb-secondary-cursor))
  (when (cursor-initialized-p cursor)
    (with-buffer-streams (key-buf pkey-buf value-buf)
      (multiple-value-bind (key pkey val)
          (db-cursor-pmove-buffered (sap cursor)
                                    key-buf pkey-buf value-buf
                                    :current t)
        (if (and key (= (read-buffer-oid key) (cursor-oid cursor)))
            (progn (setf (cursor-initialized-p cursor) t)
                   (let ((sc (get-store (btree cursor))))
                     (values t 
                             (deserialize-object key sc)
                             (deserialize-object val sc)
                             (progn (read-buffer-oid pkey) (deserialize-object pkey sc)))))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-pfirst ((cursor rdb-secondary-cursor))
  (with-buffer-streams (key-buf pkey-buf value-buf)
    (write-buffer-oid (cursor-oid cursor) key-buf)
    (multiple-value-bind (key pkey val)
        (db-cursor-pset-buffered (sap cursor) 
                                 key-buf pkey-buf value-buf :set-range t)
      (if (and key (= (read-buffer-oid key) (cursor-oid cursor)))
          (progn (setf (cursor-initialized-p cursor) t)
                 (let ((sc (get-store (btree cursor))))
                   (values t
                           (deserialize-object key sc)
                           (deserialize-object val sc)
                           (progn (read-buffer-oid pkey) (deserialize-object pkey sc)))))
          (setf (cursor-initialized-p cursor) nil)))))

;; A bit of a hack.....
(defmethod cursor-plast ((cursor rdb-secondary-cursor))
  (let ((sc (get-store (btree cursor))))
    (with-buffer-streams (key-buf pkey-buf value-buf)
      (write-buffer-oid (+ (cursor-oid cursor) 1) key-buf)
      (if (txn-iter-set (sap cursor) 
                        key-buf value-buf) ; :set-range t
          (progn (reset-buffer-stream key-buf)
                 (reset-buffer-stream value-buf)
                 (multiple-value-bind (key pkey val)
                     (db-cursor-pmove-buffered (sap cursor) key-buf pkey-buf value-buf :prev t)
                   (if (and key (= (read-buffer-oid key) (cursor-oid cursor)))
                       (progn
                         (setf (cursor-initialized-p cursor) t)
                         (values t 
                                 (deserialize-object key sc)
                                 (deserialize-object val sc)
                                 (progn (read-buffer-oid pkey) 
                                        (deserialize-object pkey sc))))
                       (setf (cursor-initialized-p cursor) nil))))
          (multiple-value-bind (key pkey val)
              (db-cursor-pmove-buffered (sap cursor) key-buf
                                        pkey-buf value-buf :last t)
            (if (and key (= (read-buffer-oid key) (cursor-oid cursor)))
                (progn
                  (setf (cursor-initialized-p cursor) t)
                  (values t (deserialize-object key sc)
                          (deserialize-object val sc)
                          (progn (read-buffer-oid pkey) (deserialize-object pkey sc))))
                (setf (cursor-initialized-p cursor) nil)))))))

(defmethod cursor-pnext ((cursor rdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf pkey-buf value-buf)
        (multiple-value-bind (key pkey val)
            (db-cursor-pmove-buffered (sap cursor) 
                                      key-buf pkey-buf value-buf :next t)
          (if (and key (= (read-buffer-oid key) (cursor-oid cursor)))
              (let ((sc (get-store (btree cursor))))
                (values t (deserialize-object key sc)
                        (deserialize-object val sc)
                        (progn (read-buffer-oid pkey) (deserialize-object pkey sc))))
              (setf (cursor-initialized-p cursor) nil))))
      (cursor-pfirst cursor)))

(defmethod cursor-pprev ((cursor rdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf pkey-buf value-buf)
        (multiple-value-bind (key pkey val)
            (db-cursor-pmove-buffered (sap cursor)
                                      key-buf pkey-buf value-buf :prev t)
          (if (and key (= (read-buffer-oid key) (cursor-oid cursor)))
              (let ((sc (get-store (btree cursor))))
                (values t (deserialize-object key sc)
                        (deserialize-object val sc)
                        (progn (read-buffer-oid pkey) (deserialize-object pkey sc))))
              (setf (cursor-initialized-p cursor) nil))))
      (cursor-plast cursor)))

(defmethod cursor-pset ((cursor rdb-secondary-cursor) key)
  (let ((sc (get-store (btree cursor))))
    (with-buffer-streams (key-buf pkey-buf value-buf)
      (write-buffer-oid (cursor-oid cursor) key-buf)
      (serialize-object key key-buf sc)
      (multiple-value-bind (k pkey val)
          (db-cursor-pset-buffered (sap cursor)
                                   key-buf pkey-buf value-buf :set t)
        (if k
            (progn
              (setf (cursor-initialized-p cursor) t)
              (values t key (deserialize-object val sc)
                      (progn (read-buffer-oid pkey) 
                             (deserialize-object pkey sc))))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-pset-range ((cursor rdb-secondary-cursor) key)
  (let ((sc (get-store (btree cursor))))
    (with-buffer-streams (key-buf pkey-buf value-buf)
      (write-buffer-oid (cursor-oid cursor) key-buf)
      (serialize-object key key-buf sc)
      (multiple-value-bind (k pkey val)
          (db-cursor-pset-buffered (sap cursor)
                                   key-buf pkey-buf value-buf :set-range t)
        (if (and k (= (read-buffer-oid k) (cursor-oid cursor)))
            (progn (setf (cursor-initialized-p cursor) t)
                   (values t (deserialize-object k sc)
                           (deserialize-object val sc)
                           (progn (read-buffer-oid pkey) (deserialize-object pkey sc))))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-pget-both ((cursor rdb-secondary-cursor) key pkey)
  (with-buffer-streams (key-buf pkey-buf value-buf)
    (let ((primary-oid (oid (primary (btree cursor))))
          (sc (get-store (btree cursor))))
      (write-buffer-oid (cursor-oid cursor) key-buf)
      (serialize-object key key-buf sc)
      (write-buffer-oid primary-oid pkey-buf)
      (serialize-object pkey pkey-buf sc)
      (multiple-value-bind (k p val)
          (db-cursor-pget-both-buffered (sap cursor)
                                        key-buf pkey-buf value-buf :get-both t)
        (declare (ignore p))
        (if k
            (progn (setf (cursor-initialized-p cursor) t)
                   (values t key (deserialize-object val sc) pkey))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-pget-both-range ((cursor rdb-secondary-cursor) key pkey)
  (with-buffer-streams (key-buf pkey-buf value-buf)
    (let ((primary-oid (oid (primary (btree cursor))))
          (sc (get-store (btree cursor))))
      (write-buffer-oid (cursor-oid cursor) key-buf)
      (serialize-object key key-buf sc)
      (write-buffer-oid primary-oid pkey-buf)
      (serialize-object pkey pkey-buf sc)
      (multiple-value-bind (k p val)
          (db-cursor-pget-both-buffered (sap cursor) key-buf 
                                        pkey-buf value-buf :get-both-range t)
        (if k
            (progn (setf (cursor-initialized-p cursor) t)
                   (values t key (deserialize-object val sc)
                           (progn (read-buffer-oid p) (deserialize-object p sc))))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-delete ((cursor rdb-secondary-cursor))
  "Delete by cursor: deletes ALL secondary index values."
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf pkey-buf value-buf)
        (multiple-value-bind (key pkey val)
            (db-cursor-pmove-buffered (sap cursor) key-buf pkey-buf
                                      value-buf :current t)
          (declare (ignore val))
          (when (and key (= (read-buffer-oid key) (cursor-oid cursor))
                     (= (read-buffer-oid pkey) (oid (primary
                                              (btree cursor)))))
            (delete-key (deserialize-object pkey (get-store (btree cursor)))
                        (primary (btree cursor))))
          (setf (cursor-initialized-p cursor) nil)))
      (error "Can't delete with uninitialized cursor!")))

(defmethod cursor-next-dup ((cursor rdb-secondary-cursor))
  (when (cursor-initialized-p cursor)
    (with-buffer-streams (key-buf value-buf)
      (multiple-value-bind (key val) (iter-move :next (sap cursor) key-buf value-buf)
        ;; TODO 2026-08-26: just validate the iterator here?
        (if (and key (= (read-buffer-oid key) (cursor-oid cursor)))
            (values t (deserialize-object key (get-store (btree cursor))) 
                    (deserialize-object val (get-store (btree cursor))))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-next-nodup ((cursor rdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf value-buf)
        (multiple-value-bind (key val)
            (iter-move :next (sap cursor)
                       key-buf value-buf) ; :next-nodup t
          (if (and key (= (read-buffer-oid key) (cursor-oid cursor)))
              (values t (deserialize-object key (get-store (btree cursor))) 
                      (deserialize-object val (get-store (btree cursor))))
              (setf (cursor-initialized-p cursor) nil))))
      (cursor-first cursor)))

(defmethod cursor-prev-nodup ((cursor rdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf value-buf)
        (multiple-value-bind (key val)
            (the (values (or null buffer-stream) 
                         (or null buffer-stream))
                 (iter-move :prev (sap cursor)
                            key-buf value-buf)) ; :prev-nodup t
          (if (and key (= (read-buffer-oid key) (cursor-oid cursor)))
              (values t (deserialize-object key (get-store (btree cursor))) 
                      (deserialize-object val (get-store (btree cursor))))
              (setf (cursor-initialized-p cursor) nil))))
      (cursor-last cursor)))

(defmethod cursor-pnext-dup ((cursor rdb-secondary-cursor))
  (when (cursor-initialized-p cursor)
    (with-buffer-streams (key-buf pkey-buf value-buf)
      (multiple-value-bind (key pkey val)
          (the (values (or null buffer-stream) 
                       (or null buffer-stream)
                       (or null buffer-stream))
               (db-cursor-pmove-buffered (sap cursor)
                                         key-buf pkey-buf value-buf :next-dup t))
        (if (and key (= (read-buffer-oid key) (cursor-oid cursor)))
            (the (values t t t t)
                 (values t (deserialize-object key (get-store (btree cursor)))
                         (deserialize-object val (get-store (btree cursor)))
                         (progn (read-buffer-oid pkey) (deserialize-object pkey (get-store (btree cursor))))))
            (the null (setf (cursor-initialized-p cursor) nil)))))))

(defmethod cursor-pnext-nodup ((cursor rdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf pkey-buf value-buf)
        (multiple-value-bind (key pkey val)
            (db-cursor-pmove-buffered (sap cursor) key-buf
                                      pkey-buf value-buf :next-nodup t)
          (if (and key (= (read-buffer-oid key) (cursor-oid cursor)))
              (values t (deserialize-object key (get-store (btree cursor))) 
                      (deserialize-object val (get-store (btree cursor)))
                      (progn (read-buffer-oid pkey) (deserialize-object pkey (get-store (btree cursor)))))
              (setf (cursor-initialized-p cursor) nil))))
      (cursor-pfirst cursor)))

(defmethod cursor-pprev-nodup ((cursor rdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf pkey-buf value-buf)
        (multiple-value-bind (key pkey val)
            (db-cursor-pmove-buffered (sap cursor) key-buf
                                      pkey-buf value-buf :prev-nodup t)
          (if (and key (= (read-buffer-oid key) (cursor-oid cursor)))
              (values t (deserialize-object key (get-store (btree cursor)))
                      (deserialize-object val (get-store (btree cursor)))
                      (progn (read-buffer-oid pkey)
                             (deserialize-object pkey (get-store (btree cursor)))))
              (setf (cursor-initialized-p cursor) nil))))
      (cursor-plast cursor)))


;; Duplicated btrees
(defclass rdb-dup-btree (dup-btree rdb-btree) ()
  ;;  (:metaclass persistent-metaclass)
  (:documentation "A RocksDB implementation of the duplicate btree"))

(defmethod make-dup-btree ((sc rdb-store))
  (make-instance 'rdb-dup-btree :store sc))

(defmethod get-value (key (bt rdb-dup-btree))
  (let ((sc (get-store bt)))
    (with-buffer-streams (key-buf)
      (write-buffer-oid (oid bt) key-buf)
      (serialize-object key key-buf sc)
      ;; used to be buffered.. test
      (let ((buf (txn-get key-buf :cf (db (btree sc)))))
        (if buf (values (deserialize-object buf sc) T)
            (values nil nil))))))

(defmethod existsp (key (bt rdb-dup-btree))
  (let ((sc (get-store bt)))
    (with-buffer-streams (key-buf value-buf)
      (write-buffer-oid (oid bt) key-buf)
      (serialize-object key key-buf sc)
      (let ((buf (txn-get key-buf :cf (db (btree sc)))))
        (if buf t
            nil)))))

;; This is the only difference with the rdb-btree -- I think that means 
;; the other methods can be removed.
(defmethod (setf get-value) (value key (bt rdb-dup-btree))
  (let ((sc (get-store bt)))
    (with-buffer-streams (key-buf value-buf)
      (write-buffer-oid (oid bt) key-buf)
      (serialize-object key key-buf sc)
      (serialize-object value value-buf sc)
      (txn-put key-buf value-buf
               :cf (db (btree sc))
               ;; :no-dup t
               )))
  value)

(defmethod delete-key (key (bt rdb-dup-btree) &key)
  (let ((sc (get-store bt)))
    (with-buffer-streams (key-buf)
      (write-buffer-oid (oid bt) key-buf)
      (serialize-object key key-buf sc)
      (txn-delete key-buf :cf (db (btree sc))))))

(defclass rdb-dup-cursor (rdb-cursor) ()
  (:documentation "Cursor for traversing rdb secondary index-table."))

(defmethod make-cursor ((bt rdb-dup-btree))
  "Make a secondary-cursor from a secondary index."
  (let ((sc (get-store bt)))
    (make-instance 'rdb-dup-cursor
      :btree bt
      :sap (or (and *transaction* (txn-iter)) (btree sc))
      :oid (oid bt))))

(defmethod cursor-next-nodup ((cursor rdb-dup-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf value-buf)
        (multiple-value-bind (key val)
            (iter-move :next (sap cursor) key-buf value-buf) ; :next-nodup t
          (if (and key (= (read-buffer-oid key) (cursor-oid cursor)))
              (values t (deserialize-object key (get-store (btree cursor))) 
                      (deserialize-object val (get-store (btree cursor))))
              (setf (cursor-initialized-p cursor) nil))))
      (cursor-first cursor)))

(defmethod cursor-delete ((cursor rdb-dup-cursor))
  (if (cursor-initialized-p cursor)
      (progn (cursor-delete (sap cursor))
             (setf (cursor-initialized-p cursor) nil))
      (error "Can't delete with uninitialized cursor!")))

;;; Open/Close
(defmethod open-store ((store rdb-store) &key recover id)
  (setq *store* store)
  (setf (slot-value store 'path) (path store)
        (slot-value store 'options) (options store)
        (slot-value store 'transactiondb-options) (transactiondb-options store))
  ;; if pre-existing, load options and all column-families into COLUMNS slot
  (let ((newp))
    (cond 
      (recover (restore store recover :id id))
      ((open-db store) ; non-nil value indicates a new instance
       (setf newp t)
       (set-database-version store (db (make-column store :name "metadata")))
       ;; create column-families
       (make-column store :name "oids")
       (make-column store :name "seqs" :class 'counter-column-family)
       ;; needs lisp-comparator for following
       (make-column store :name "btrees")
       (make-column store :name "bbtrees")
       (make-column store :name "index")
       (make-column store :name "rindex")))
    ;; the default column family serves as the slot-value data store
    (setf (store-metadata store) (find-column "metadata" store)
          (btrees store) (find-column "btrees" store)
          ;; TODO 2026-08-16: dup-btree -> itree?
          (dup-btrees store) (find-column "bbtrees" store)
          (seqs store) 
          (let ((col (find-column "seqs" store)))
            (if newp
                (progn 
                  ;; initialize two counter values
                  (setf (data col) 
                        (the (simple-array word (*)) (make-array 2 :element-type 'word :initial-element 0)))
                  (save col :db (db store)))
                (init col :db (db store))))
          (oids store) (find-column "oids" store)
          (index store) (find-column "index" store)
          (rindex store) (find-column "rindex" store))
    ;; set the store version number
    (destructuring-bind (maj min inc) (version store)
      (setf (version store)
            (+ (* 100 maj)
               (* 10 min)
               inc)))
    (with-transaction (:db store)
      ;; btree initialization
      (setf 
       (slot-value store 'root) (make-instance 'rdb-btree :from-oid -1 :store store)
       (slot-value store 'store::index-root) (make-instance 'rdb-btree :from-oid -2 :store store))
      ;; TODO 2026-08-23: 
      ;; (inspect *store*)
      (setf
       (slot-value store 'store::instance-index)
       (if newp
           (make-instance 'rdb-indexed-btree :from-oid -3 :store store :index (make-hash-table))
           (make-instance 'rdb-indexed-btree :from-oid -3 :store store)))
      ;; (inspect store)
      (setf
       (slot-value store 'store::schema-index)
       (if newp
           (make-instance 'rdb-indexed-btree :from-oid -4 :store store :index (make-hash-table))
           (make-instance 'rdb-indexed-btree :from-oid -4 :store store))))
    store))

(defmethod close-store ((store rdb-store))
  "Close the underlying RocksDB instance."
    (setf 
     (slot-value store 'store::index-root) nil
     (slot-value store 'store::schema-index) nil
     (slot-value store 'store::instance-index) nil
     (slot-value store 'root) nil)
    (flush-instance-cache store)
    (setf (seqs store) nil
          (oids store) nil
          (dup-btrees store) nil
          (btrees store) nil
          (store-metadata store) nil
          (index store) nil
          (rindex store) nil)
  (shutdown-db store))

;;; IDs
(defmethod next-cid ((self rdb-store))
  (sb-ext:atomic-incf (aref (slot-value (seqs self) 'data) 0)))
(defmethod next-oid ((self rdb-store))
  (sb-ext:atomic-incf (aref (slot-value (seqs self) 'data) 1)))

(defmethod default-class-id (type (sc rdb-store))
  (ecase type
    (rdb-btree 1)
    (rdb-dup-btree 2)
    (rdb-indexed-btree 3)
    (rdb-btree-index 4)))

(defmethod default-class-id-type (cid (sc rdb-store))
  (case cid
    (1 'rdb-btree)
    (2 'rdb-dup-btree)
    (3 'rdb-indexed-btree)
    (4 'rdb-btree-index)))

(defmethod reserved-oid-p ((sc rdb-store) oid)
  (< oid 2))

;;; slot protocol
;; TODO 2024-11-07:
(defmethod stored-slot-reader ((self rdb-store) instance name &optional oids-only)
  (declare (ignore oids-only))
  (trace! "stored-slot-reader" self instance name)
  (ensure-transaction (:db self)
    (with-buffer-streams (kbuf vbuf)
      (write-buffer-fixnum32 (the fixnum (oid instance)) kbuf)
      (serialize-object name kbuf self)
      (let ((buf (txn-get kbuf)))
        (trace! "slot value: " buf)
        (if buf 
            (deserialize-object buf self)
            (slot-unbound (class-of instance) instance name))))))

(defmethod stored-slot-writer ((self rdb-store) new-value instance name)
  (ensure-transaction (:db self)
    (with-buffer-streams (key-buf value-buf)
      (write-buffer-fixnum32 (oid instance) key-buf)
      (serialize-object name key-buf self)
      (serialize-object new-value value-buf self)
      (txn-put key-buf value-buf)
      new-value)))

(defmethod stored-slot-boundp ((self rdb-store) instance name)
  (ensure-transaction (:db self)
    (with-buffer-streams (key-buf)
      (write-buffer-fixnum32 (oid instance) key-buf)
      (serialize-object name key-buf self)
      (let ((buf (txn-get key-buf)))
        (if buf t nil)))))
    
(defmethod stored-slot-makunbound ((self rdb-store) instance name)
  (ensure-transaction (:db self)
    (with-buffer-streams (key-buf)
      (write-buffer-fixnum32 (oid instance) key-buf)
      (serialize-object name key-buf self)
      (txn-delete key-buf))))

;;; Transactions
;; TODO 2026-08-21: 
(defmethod execute ((store rdb-store) txn-fn &key transaction handler)
  (with-retry-restart (:msg "Retry transaction execution.")
    (let ((ret) (ok) (txn (transaction store :transaction transaction)))
      (let ((*transaction* txn)
            (*store* store)
            (*db* store))
        (declare (special *transaction* *store* *db*))
        (catch 'transaction
          (unwind-protect
               (handler-bind
                   ((condition 
                      (lambda (c)
                        (when (and handler (funcall handler c))
                          (commit txn)
                          (setq ok t))
                        (signal c))))
                 (setf ret (multiple-value-list (funcall txn-fn)))
                 (with-errptr e (rocksdb-transaction-commit txn e))
                 (setq ok t))
            (unless ok (%abort-transaction txn)))))
      (when ok (values-list ret)))))
          
                               


  
                                         
