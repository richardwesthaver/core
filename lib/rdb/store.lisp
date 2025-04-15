;;; store.lisp --- RocksDB Store

;; OBJ/STORE implementation for RocksDB

;;; Code:
(in-package :rdb)

;; what should the underlying datastructure be? transaction-db wbwi or cf?
(defclass rdb-btree (btree) ()
  (:documentation "A RocksDB implementation of a BTree."))

(defclass rdb-store (store rdb-database)
  ((oid-seq :accessor oid-seq)
   (cid-seq :accessor cid-seq)
   (logger :initform (default-logger) :initarg :logger :accessor logger))
   (:default-initargs
    :spec '(:rdb)
    :db (make-db :rocksdb :opts (default-rdb-opts))
    :columns (make-array 0 :element-type 'rdb-column-family
                           :adjustable t
                           :fill-pointer t)
    ;; :instance-table (make-instance 'rdb-column-family :type '(oid . cid))
    ;; :instance-class-index (make-instance 'rdb-column-family :type '(cid . oid))
    ;; :root
    ;; :schema-table (make-hash-table :size 100 :weakness :value)
    ;; :schema-name-index (make-hash-table :size 100 :test 'equal :weakness :value)
    ;; :index-table
    ;; :instance-table
    ;; :instance-class-index
    )
   (:documentation "A RocksDB STORE. Note that the default column family is used to store
serialized object schemas."))

(defmethod build-btree ((st rdb-store))
  (make-instance 'rdb-btree :store st))

;; (build-btree (make-instance 'rdb-store))

(defun rdb-store-spec-p (spec)
  (and (eq (first spec) :rdb)
       (typecase (second spec)
         (pathname t)
         (string t)
         (t nil))))

(defmethod get-value (key (bt rdb-btree))
  "Getting a value from a plain RDB-BTREE will fetch the value directly from (DB *STORE*)."
  (let ((sc (get-store bt)))
    (ensure-transaction (:store sc)
      (with-static-stream (key-buf)
        (write-oid (oid bt) key-buf)
        (ser key key-buf sc)
        (let ((buf (db-get-key-buffered
                    (btree sc)
                    :transaction (current-transaction sc))))
          (if buf (values (deserialize buf sc) t)
              (values nil nil)))))))

(defmethod existsp (key (bt rdb-btree))
  (let ((sc (get-store bt)))
    (ensure-transaction (:store sc)
      (with-static-stream (key-buf)
      (write-oid (oid bt) key-buf)
      (ser key key-buf sc)
      (let ((buf (db-get-key-buffered 
                  (btree sc)
                  :transaction (current-transaction sc))))
          (if buf t
              nil))))))

(defmethod (setf get-value) (value key (bt rdb-btree))
    (let ((sc (get-store bt)))
      (ensure-transaction (:store sc)
        (with-static-streams ((key-buf) (value-buf))
          (write-oid (oid bt) key-buf)
          (ser key key-buf sc)
          (ser value value-buf sc)
          (db-put-buffered (btree sc)
                           :transaction (current-transaction sc))))
      value))

(defmethod delete-key (key (bt rdb-btree) &key)
  (let ((sc (get-store bt)) )
    (with-static-stream (key-buf)
      (ensure-transaction (:store sc)
        (write-oid (oid bt) key-buf)
        (ser key key-buf sc)
        (db-delete-buffered (btree sc)
                            :transaction (current-transaction sc))))))

(defmethod optimize-layout ((bt rdb-btree) &key (freelist-only t) (free-space nil) &allow-other-keys)
  (optimize-layout (get-store bt)
                   :start-key (oid bt)
                   :end-key (oid bt)
                   :freelist-only freelist-only
                   :free-space free-space))

(defclass rdb-indexed-btree (indexed-btree rdb-btree)
  ((index-table :accessor index-table :initarg :index-table :initform (make-hash-table))
   (index-cache-table :accessor index-cache-table :transient t))
  (:metaclass stored-class)
  (:documentation "A RDB-based BTree supports secondary index-table."))

(defmethod index-cache-table ((instance rdb-indexed-btree))
  ;; Lazily load the index-cache-table to avoid bootstrapping issues: If
  ;; we do not lazy-load the index-table cache, it we attempt to
  ;; initialize it before the instance-table is available (thus we
  ;; cannot map oids to classes -- deserialize does not really work
  ;; for complex objects).  -- Red Daly 07/10/2010
  (aif (slot-value instance 'index-cache-table)
       it
       (setf (index-cache-table instance) (index-table instance))))

(defmethod shared-initialize :after ((instance rdb-indexed-btree) slot-names
                                     &rest rest)
  (declare (ignore slot-names rest))
  (setf (index-cache-table instance) nil))

(defmethod build-indexed-btree ((sc rdb-store))
  (make-instance 'rdb-indexed-btree :sc sc))

(defmethod build-btree-index ((sc rdb-store) &key primary key-form &allow-other-keys)
  (make-instance 'rdb-btree-index :primary primary :key-form key-form :sc sc))

(defmethod add-index ((bt rdb-indexed-btree) &key index-name key-form (populate t))
  (let ((sc (get-store bt)))
;; Setting the value of *store* is unfortunately
;; absolutely required at present, I think because the copying 
;; of objects is calling "make-instance" without an argument.
;; I am sure I can find a way to make this cleaner, somehow.
    (if (and (not (null index-name))
             (symbolp index-name)
             (or (symbolp key-form) (listp key-form)))
        ;; Can it be that this fails?
        (let ((index
               (ensure-transaction (:store sc)
                 (let ((ht (index-table bt))
                       (index (build-btree-index sc 
                                                 :primary bt 
                                                 :key-form key-form)))
                   (setf (gethash index-name (index-cache-table bt)) index)
                   (setf (gethash index-name ht) index)
                   (setf (index-table bt) ht)
                   index))))
          (when populate (populate bt index))
          index)
        (error "Invalid index initargs!"))))

(defmethod populate ((bt rdb-indexed-btree) index)
  (let ((sc (get-store bt)))
    (with-static-streams ((primary-buf) (secondary-buf))
      (flet ((index (key skey)
               (write-oid (oid bt) primary-buf)
               (ser key primary-buf sc)
               (write-oid (oid index) secondary-buf)
               (ser skey secondary-buf sc)
               ;; should silently do nothing if
               ;; the key/value already exists
               (db-put-buffered 
                (index-table sc)
                secondary-buf primary-buf
                :transaction (current-transaction sc))
               (reset-static-stream primary-buf)
               (reset-static-stream secondary-buf)))
        (let ((key-fn (key-fn index))
              (last-key nil)
              (continue t))
          (loop while continue
             do
             (ensure-transaction (:store sc)
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
                          (when index? (index k skey))))
                      (multiple-value-bind (valid? k v) (cursor-next cursor)
                        (declare (ignore v))
                        (if valid? 
                            (setf last-key k)
                            (setf continue nil))))))))))))


(defmethod map-index-table (fn (bt rdb-indexed-btree))
  (maphash fn (index-cache-table bt)))

(defmethod get-index ((bt rdb-indexed-btree) index-name)
  (gethash index-name (index-cache-table bt)))

(defmethod remove-index ((bt rdb-indexed-btree) index-name)
  (remhash index-name (index-cache-table bt))
  (let ((index-table (index-table bt)))
    (remhash index-name index-table)
    (setf (index-table bt) index-table)))

(defmethod (setf get-value) (value key (bt rdb-indexed-btree))
  "Set a key / value pair, and update secondary index-table."
  (let ((sc (get-store bt)))
    (let ((index-table (index-cache-table bt)))
      (with-static-streams ((key-buf) (value-buf) (secondary-buf))
        (write-oid (oid bt) key-buf)
        (ser key key-buf sc)
        (ser value value-buf sc)
        (ensure-transaction (:store sc)
          (db-put-buffered (btree sc)
                           key-buf value-buf
                           :transaction (current-transaction sc))
          ;; Manually write value into secondary index
          (loop for index being the hash-value of index-table
             do
             (multiple-value-bind (index? secondary-key)
                 (funcall (key-fn index) index key value)
               (when index?
                 ;; Insert
                 (write-oid (oid index) secondary-buf)
                 (ser secondary-key secondary-buf sc)
                 (db-put-buffered (index-table sc)
                                  secondary-buf key-buf
                                  :no-dup t
                                  :transaction (current-transaction sc))
                 (reset-static-stream secondary-buf))))
          value)))))

(defmethod delete-key (key (bt rdb-indexed-btree) &key)
  "Remove a key / value pair, and update secondary index-table."
  (let ((sc (get-store bt)))
      (with-static-streams ((key-buf) (secondary-buf))
        (write-oid (oid bt) key-buf)
        (ser key key-buf sc)
        (ensure-transaction (:store sc)
          (let ((value (get-value key bt)))
            (when value
              (let ((index-table (index-cache-table bt)))
                (loop 
                   for index being the hash-value of index-table
                   do
                   (multiple-value-bind (index? secondary-key)
                       (funcall (key-fn index) index key value)
                     (when index?
                       (write-oid (oid index) secondary-buf)
                       (ser secondary-key secondary-buf sc)
                       ;; need to remove kv pairs with a cursor! --
                       ;; this is a C performance hack
                       (db-delete-kv-buffered 
                        (index-table (get-store bt))
                        secondary-buf key-buf
                        :transaction (current-transaction sc))
                       (reset-static-stream secondary-buf))))
                (db-delete-buffered (btree (get-store bt))
                                    key-buf
                                    :transaction (current-transaction sc)))))))))

;; This also needs to build the correct kind of index, and 
;; be the correct kind of btree...

(defclass rdb-btree-index (btree-index rdb-btree)
  ()
  (:metaclass stored-class)
  (:documentation "A RDB-based BTree supports secondary index-table."))

(defmethod get-value (key (bt rdb-btree-index))
  "Get the value in the primary DB from a secondary key."
  (let ((sc (get-store bt)))
    (with-static-streams ((key-buf) (value-buf))
      (write-oid (oid bt) key-buf)
      (ser key key-buf sc)
      (let ((buf (db-get-key-buffered 
                  (index-table-assoc sc)
                  key-buf value-buf
                  :transaction (current-transaction sc))))
        (if buf (values (deserialize buf sc) T)
            (values nil nil))))))

(defmethod get-primary-key (key (bt btree-index))
  (let ((sc (get-store bt)))
    (with-static-streams ((key-buf) (value-buf))
      (write-oid (oid bt) key-buf)
      (ser key key-buf sc)
      (let ((buf (db-get-key-buffered 
                  (index-table sc)
                  key-buf value-buf
                  :transaction (current-transaction sc))))
        (if buf 
            (let ((oid (read-oid buf)))
              (values (deserialize buf sc) oid))
            (values nil nil))))))

(defclass rdb-cursor (cursor)
  ((handle :accessor cursor-handle :initarg :handle))
  (:documentation "A cursor for traversing (primary) RDB-BTrees."))

(defmethod make-cursor ((bt rdb-btree))
  "Make a cursor from a btree."
  (let ((sc (get-store bt)))
    (make-instance 'rdb-cursor 
                   :btree bt
                   :handle (db-cursor (btree sc)
                                      :transaction (current-transaction sc))
                   :oid (oid bt))))

(defmethod cursor-close ((cursor rdb-cursor))
  (cursor-close (cursor-handle cursor))
  (setf (cursor-initialized-p cursor) nil))

(defmethod cursor-duplicate ((cursor rdb-cursor))
  (make-instance (type-of cursor)
                 :initialized-p (cursor-initialized-p cursor)
                 :oid (cursor-oid cursor)
                 :handle (cursor-duplicate 
                          (cursor-handle cursor) 
                          :position (cursor-initialized-p cursor))))

(defmethod cursor-current ((cursor rdb-cursor))
  (when (cursor-initialized-p cursor)
    (let ((sc (get-store (btree cursor))))
      (with-static-streams ((key-buf) (value-buf))
        (multiple-value-bind (key val)
            (cursor-move-buffered (cursor-handle cursor) key-buf value-buf
                                     :current t)
          (if (and key (= (read-oid key) (cursor-oid cursor)))
              (progn (setf (cursor-initialized-p cursor) t)
                     (values t (deserialize key sc)
                             (deserialize val sc)))
              (setf (cursor-initialized-p cursor) nil)))))))

(defmethod cursor-first ((cursor rdb-cursor))
  (let ((sc (get-store (btree cursor))))
    (with-static-streams ((key-buf) (value-buf))
      (write-oid (cursor-oid cursor) key-buf)
      (multiple-value-bind (key val)
          (cursor-set-buffered (cursor-handle cursor) 
                                  key-buf value-buf :set-range t)
        (if (and key (= (read-oid key) (cursor-oid cursor)))
            (progn (setf (cursor-initialized-p cursor) t)
                   (values t 
                           (deserialize key sc)
                           (deserialize val sc)))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-last ((cursor rdb-cursor))
  "A fast cursor last, but a bit 'hackish' by exploiting oid ordering"
  (let ((sc (get-store (btree cursor))))
  (with-static-streams ((key-buf) (value-buf))
    ;; Go to the first element of the next btree
    (write-oid (+ (cursor-oid cursor) 1) key-buf)
    (if (cursor-set-buffered (cursor-handle cursor)
                                key-buf value-buf :set-range t)
        (progn (reset-static-stream key-buf)
               (reset-static-stream value-buf)
               (multiple-value-bind (key val)
                   (cursor-move-buffered (cursor-handle cursor) 
                                            key-buf value-buf :prev t)
                 (if (and key (= (read-oid key) (cursor-oid cursor)))
                     (progn
                       (setf (cursor-initialized-p cursor) t)
                       (values t (deserialize key sc)
                                 (deserialize val sc)))
                     (setf (cursor-initialized-p cursor) nil))))
        (multiple-value-bind (key val)
            (cursor-move-buffered (cursor-handle cursor) key-buf
                                     value-buf :last t)
          (if (and key (= (read-oid key) (cursor-oid cursor)))
              (progn
                (setf (cursor-initialized-p cursor) t)
                (values t (deserialize key sc)
                        (deserialize val sc )))
              (setf (cursor-initialized-p cursor) nil)))))))

(defmethod cursor-next ((cursor rdb-cursor))
  (if (cursor-initialized-p cursor)
      (let ((sc (get-store (btree cursor))))
        (with-static-streams ((key-buf) (value-buf))
          (multiple-value-bind (key val)
              (the (values (or null static-stream)
                           (or null static-stream))
                (cursor-move-buffered (cursor-handle cursor) 
                                         key-buf value-buf :next t))
            (if (and key (= (read-oid key) (cursor-oid cursor)))
                (the (values t t t)
                  (values t (deserialize key sc)
                          (deserialize val sc)))
                (the null (setf (cursor-initialized-p cursor) nil))))))
      (the t (cursor-first cursor))))

(defmethod cursor-prev ((cursor rdb-cursor))
  (if (cursor-initialized-p cursor)
      (let ((sc (get-store (btree cursor))))
        (with-static-streams ((key-buf) (value-buf))
          (multiple-value-bind (key val)
              (cursor-move-buffered (cursor-handle cursor)
                                       key-buf value-buf :prev t)
            (if (and key (= (read-oid key) (cursor-oid cursor)))
                (values t (deserialize key sc)
                        (deserialize val sc))
                (setf (cursor-initialized-p cursor) nil)))))
      (cursor-last cursor)))

(defmethod cursor-set ((cursor rdb-cursor) key)
  (let ((sc (get-store (btree cursor))))
  (with-static-streams ((key-buf) (value-buf))
    (write-oid (cursor-oid cursor) key-buf)
    (ser key key-buf sc)
    (multiple-value-bind (k val)
        (cursor-set-buffered (cursor-handle cursor)
                             key-buf value-buf :set t)
      (if k
          (progn
            (setf (cursor-initialized-p cursor) t)
            (values t key (deserialize val sc)))
          (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-set-range ((cursor rdb-cursor) key)
  (let ((sc (get-store (btree cursor))))
  (with-static-streams ((key-buf) (value-buf))
    (write-oid (cursor-oid cursor) key-buf)
    (ser key key-buf sc)
    (multiple-value-bind (k val)
        (cursor-set-buffered (cursor-handle cursor)
                                key-buf value-buf :set-range t)
      (if (and k (= (read-oid k) (cursor-oid cursor)))
          (progn (setf (cursor-initialized-p cursor) t)
                 (values t (deserialize k sc)
                         (deserialize val sc)))
          (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-get-both ((cursor rdb-cursor) key value)
  (let ((sc (get-store (btree cursor))))
  (with-static-streams ((key-buf) (value-buf))
    (write-oid (cursor-oid cursor) key-buf)
    (ser key key-buf sc)
    (ser value value-buf sc)
    (multiple-value-bind (k v)
        (cursor-get-both-buffered (cursor-handle cursor)
                                     key-buf value-buf :get-both t)
      (declare (ignore v))
      (if k
          (progn (setf (cursor-initialized-p cursor) t)
                 (values t key value))
          (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-get-both-range ((cursor rdb-cursor) key value)
  (let ((sc (get-store (btree cursor))))
  (with-static-streams ((key-buf) (value-buf))
    (write-oid (cursor-oid cursor) key-buf)
    (ser key key-buf sc)
    (ser value value-buf sc)
    (multiple-value-bind (k v)
        (cursor-get-both-buffered (cursor-handle cursor)
                                     key-buf value-buf :get-both-range t)
      (if k
          (progn (setf (cursor-initialized-p cursor) t)
                 (values t key (deserialize v sc)))
          (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-delete ((cursor rdb-cursor))
  (if (cursor-initialized-p cursor)
      (with-static-streams ((key-buf) (value-buf))
        (multiple-value-bind (key val)
            (cursor-move-buffered (cursor-handle cursor) key-buf value-buf
                                     :current t)
          (declare (ignore val))
          (when (and key (= (read-oid key) (cursor-oid cursor)))
            ;; in case of a secondary index this should delete everything
            ;; as specified by the RDB docs.
            (delete-key (deserialize key (get-store (btree cursor)))
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
            (with-static-streams ((key-buf) (value-buf))
              (multiple-value-bind (k v)
                  (cursor-move-buffered (cursor-handle cursor) key-buf 
                                           value-buf :current t)
                (declare (ignore v))
                (if (and k (= (read-oid k) (cursor-oid cursor)))
                    (progn
                      (setf (get-value (deserialize k sc) (btree cursor))
                            value)
                      (reset-static-stream key-buf) (reset-static-stream value-buf)
                      (multiple-value-bind (k v)
                          (cursor-move-buffered (cursor-handle cursor) key-buf
                                                   value-buf :next t)
                        (if (and key (= (read-oid k) (cursor-oid cursor)))
                            (values t (deserialize k sc) (deserialize v sc))
                            (setf (cursor-initialized-p cursor) nil))))
                    (setf (cursor-initialized-p cursor) nil)))))
          (error "Can't put with uninitialized cursor!"))))

;; Secondary cursors

(defclass rdb-secondary-cursor (secondary-cursor rdb-cursor) ()
  (:documentation "Cursor for traversing rdb secondary index-table."))

(defmethod make-cursor ((bt rdb-btree-index))
  "Make a secondary-cursor from a secondary index."
  (let ((sc (get-store bt)))
    (make-instance 'rdb-secondary-cursor 
                   :btree bt
                   :handle (db-cursor (index-table-assoc sc)
                                      :transaction (current-transaction sc))
                   :oid (oid bt))))

(defmethod cursor-pcurrent ((cursor rdb-secondary-cursor))
  (when (cursor-initialized-p cursor)
    (with-static-streams ((key-buf) (pkey-buf) (value-buf))
      (multiple-value-bind (key pkey val)
          (db-cursor-pmove-buffered (cursor-handle cursor)
                                    key-buf pkey-buf value-buf
                                    :current t)
        (if (and key (= (read-oid key) (cursor-oid cursor)))
            (progn (setf (cursor-initialized-p cursor) t)
                   (let ((sc (get-store (btree cursor))))
                     (values t 
                             (deserialize key sc)
                             (deserialize val sc)
                             (progn (read-oid pkey) (deserialize pkey sc)))))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-pfirst ((cursor rdb-secondary-cursor))
  (with-static-streams ((key-buf) (pkey-buf) (value-buf))
    (write-oid (cursor-oid cursor) key-buf)
    (multiple-value-bind (key pkey val)
        (db-cursor-pset-buffered (cursor-handle cursor) 
                                 key-buf pkey-buf value-buf :set-range t)
      (if (and key (= (read-oid key) (cursor-oid cursor)))
          (progn (setf (cursor-initialized-p cursor) t)
                 (let ((sc (get-store (btree cursor))))
                 (values t 
                         (deserialize key sc)
                         (deserialize val sc)
                         (progn (read-oid pkey) (deserialize pkey sc)))))
          (setf (cursor-initialized-p cursor) nil)))))

;; A bit of a hack.....
(defmethod cursor-plast ((cursor rdb-secondary-cursor))
  (let ((sc (get-store (btree cursor))))
  (with-static-streams ((key-buf) (pkey-buf) (value-buf))
    (write-oid (+ (cursor-oid cursor) 1) key-buf)
    (if (db-cursor-set-buffered (cursor-handle cursor) 
                                key-buf value-buf :set-range t)    
        (progn (reset-static-stream key-buf)
               (reset-static-stream value-buf)
               (multiple-value-bind (key pkey val)
                   (db-cursor-pmove-buffered (cursor-handle cursor) key-buf 
                                             pkey-buf value-buf :prev t)
                 (if (and key (= (read-oid key) (cursor-oid cursor)))
                     (progn
                       (setf (cursor-initialized-p cursor) t)
                       (values t 
                               (deserialize key sc)
                               (deserialize val sc)
                               (progn (read-oid pkey) 
                                      (deserialize pkey sc))))
                     (setf (cursor-initialized-p cursor) nil))))
        (multiple-value-bind (key pkey val)
            (db-cursor-pmove-buffered (cursor-handle cursor) key-buf
                                      pkey-buf value-buf :last t)
          (if (and key (= (read-oid key) (cursor-oid cursor)))
              (progn
                (setf (cursor-initialized-p cursor) t)
                (values t (deserialize key sc)
                        (deserialize val sc)
                        (progn (read-oid pkey) (deserialize pkey sc))))
              (setf (cursor-initialized-p cursor) nil)))))))

(defmethod cursor-pnext ((cursor rdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-static-streams ((key-buf) (pkey-buf) (value-buf))
        (multiple-value-bind (key pkey val)
            (db-cursor-pmove-buffered (cursor-handle cursor) 
                                     key-buf pkey-buf value-buf :next t)
          (if (and key (= (read-oid key) (cursor-oid cursor)))
              (let ((sc (get-store (btree cursor))))
                (values t (deserialize key sc)
                        (deserialize val sc)
                        (progn (read-oid pkey) (deserialize pkey sc))))
              (setf (cursor-initialized-p cursor) nil))))
      (cursor-pfirst cursor)))

(defmethod cursor-pprev ((cursor rdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-static-streams ((key-buf) (pkey-buf) (value-buf))
        (multiple-value-bind (key pkey val)
            (db-cursor-pmove-buffered (cursor-handle cursor)
                                      key-buf pkey-buf value-buf :prev t)
          (if (and key (= (read-oid key) (cursor-oid cursor)))
              (let ((sc (get-store (btree cursor))))
                (values t (deserialize key sc)
                        (deserialize val sc)
                        (progn (read-oid pkey) (deserialize pkey sc))))
              (setf (cursor-initialized-p cursor) nil))))
      (cursor-plast cursor)))

(defmethod cursor-pset ((cursor rdb-secondary-cursor) key)
  (let ((sc (get-store (btree cursor))))
    (with-static-streams ((key-buf) (pkey-buf) (value-buf))
      (write-oid (cursor-oid cursor) key-buf)
      (ser key key-buf sc)
      (multiple-value-bind (k pkey val)
          (db-cursor-pset-buffered (cursor-handle cursor)
                                   key-buf pkey-buf value-buf :set t)
        (if k
            (progn
              (setf (cursor-initialized-p cursor) t)
              (values t key (deserialize val sc)
                      (progn (read-oid pkey) 
                             (deserialize pkey sc))))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-pset-range ((cursor rdb-secondary-cursor) key)
  (let ((sc (get-store (btree cursor))))
    (with-static-streams ((key-buf) (pkey-buf) (value-buf))
      (write-oid (cursor-oid cursor) key-buf)
      (ser key key-buf sc)
      (multiple-value-bind (k pkey val)
          (db-cursor-pset-buffered (cursor-handle cursor)
                                   key-buf pkey-buf value-buf :set-range t)
        (if (and k (= (read-oid k) (cursor-oid cursor)))
            (progn (setf (cursor-initialized-p cursor) t)
                   (values t (deserialize k sc)
                           (deserialize val sc)
                           (progn (read-oid pkey) (deserialize pkey sc))))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-pget-both ((cursor rdb-secondary-cursor) key pkey)
  (with-static-streams ((key-buf) (pkey-buf) (value-buf))
    (let ((primary-oid (oid (primary (btree cursor))))
          (sc (get-store (btree cursor))))
      (write-oid (cursor-oid cursor) key-buf)
      (ser key key-buf sc)
      (write-oid primary-oid pkey-buf)
      (ser pkey pkey-buf sc)
      (multiple-value-bind (k p val)
          (db-cursor-pget-both-buffered (cursor-handle cursor)
                                        key-buf pkey-buf value-buf :get-both t)
        (declare (ignore p))
        (if k
            (progn (setf (cursor-initialized-p cursor) t)
                   (values t key (deserialize val sc) pkey))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-pget-both-range ((cursor rdb-secondary-cursor) key pkey)
  (with-static-streams ((key-buf) (pkey-buf) (value-buf))
    (let ((primary-oid (oid (primary (btree cursor))))
          (sc (get-store (btree cursor))))
      (write-oid (cursor-oid cursor) key-buf)
      (ser key key-buf sc)
      (write-oid primary-oid pkey-buf)
      (ser pkey pkey-buf sc)
      (multiple-value-bind (k p val)
          (db-cursor-pget-both-buffered (cursor-handle cursor) key-buf 
                                        pkey-buf value-buf :get-both-range t)
        (if k
            (progn (setf (cursor-initialized-p cursor) t)
                   (values t key (deserialize val sc)
                           (progn (read-oid p) (deserialize p sc))))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-delete ((cursor rdb-secondary-cursor))
  "Delete by cursor: deletes ALL secondary index values."
  (if (cursor-initialized-p cursor)
      (with-static-streams ((key-buf) (pkey-buf) (value-buf))
        (multiple-value-bind (key pkey val)
            (db-cursor-pmove-buffered (cursor-handle cursor) key-buf pkey-buf
                                      value-buf :current t)
          (declare (ignore val))
          (when (and key (= (read-oid key) (cursor-oid cursor))
                     (= (read-oid pkey) (oid (primary
                                                     (btree cursor)))))
            (delete-key (deserialize pkey (get-store (btree cursor)))
                       (primary (btree cursor))))
          (setf (cursor-initialized-p cursor) nil)))
      (error "Can't delete with uninitialized cursor!")))

(defmethod cursor-next-dup ((cursor rdb-secondary-cursor))
  (when (cursor-initialized-p cursor)
    (with-static-streams ((key-buf) (value-buf))
      (multiple-value-bind (key val)
          (db-cursor-move-buffered (cursor-handle cursor)
                                   key-buf value-buf :next-dup t)
        (if (and key (= (read-oid key) (cursor-oid cursor)))
            (values t (deserialize key (get-store (btree cursor))) 
                    (deserialize val (get-store (btree cursor))))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-next-nodup ((cursor rdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-static-streams ((key-buf) (value-buf))
        (multiple-value-bind (key val)
            (db-cursor-move-buffered (cursor-handle cursor)
                                     key-buf value-buf :next-nodup t)
          (if (and key (= (read-oid key) (cursor-oid cursor)))
              (values t (deserialize key (get-store (btree cursor))) 
                      (deserialize val (get-store (btree cursor))))
              (setf (cursor-initialized-p cursor) nil))))
      (cursor-first cursor)))

(defmethod cursor-prev-nodup ((cursor rdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-static-streams ((key-buf) (value-buf))
        (multiple-value-bind (key val)
          (the (values (or null static-stream) 
                       (or null static-stream))
            (db-cursor-move-buffered (cursor-handle cursor)
                                     key-buf value-buf :prev-nodup t))
          (if (and key (= (read-oid key) (cursor-oid cursor)))
              (values t (deserialize key (get-store (btree cursor))) 
                      (deserialize val (get-store (btree cursor))))
              (setf (cursor-initialized-p cursor) nil))))
      (cursor-last cursor)))

(defmethod cursor-pnext-dup ((cursor rdb-secondary-cursor))
  (when (cursor-initialized-p cursor)
    (with-static-streams ((key-buf) (pkey-buf) (value-buf))
      (multiple-value-bind (key pkey val)
          (the (values (or null static-stream) 
                       (or null static-stream)
                       (or null static-stream))
            (db-cursor-pmove-buffered (cursor-handle cursor)
                                    key-buf pkey-buf value-buf :next-dup t))
        (if (and key (= (read-oid key) (cursor-oid cursor)))
            (the (values t t t t)
              (values t (deserialize key (get-store (btree cursor)))
                      (deserialize val (get-store (btree cursor)))
                      (progn (read-oid pkey) (deserialize pkey (get-store (btree cursor))))))
            (the null (setf (cursor-initialized-p cursor) nil)))))))

(defmethod cursor-pnext-nodup ((cursor rdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-static-streams ((key-buf) (pkey-buf) (value-buf))
        (multiple-value-bind (key pkey val)
            (db-cursor-pmove-buffered (cursor-handle cursor) key-buf
                                      pkey-buf value-buf :next-nodup t)
          (if (and key (= (read-oid key) (cursor-oid cursor)))
              (values t (deserialize key (get-store (btree cursor))) 
                      (deserialize val (get-store (btree cursor)))
                      (progn (read-oid pkey) (deserialize pkey (get-store (btree cursor)))))
              (setf (cursor-initialized-p cursor) nil))))
      (cursor-pfirst cursor)))

(defmethod cursor-pprev-nodup ((cursor rdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-static-streams ((key-buf) (pkey-buf) (value-buf))
        (multiple-value-bind (key pkey val)
            (db-cursor-pmove-buffered (cursor-handle cursor) key-buf
                                      pkey-buf value-buf :prev-nodup t)
          (if (and key (= (read-oid key) (cursor-oid cursor)))
              (values t (deserialize key (get-store (btree cursor)))
                      (deserialize val (get-store (btree cursor)))
                      (progn (read-oid pkey)
                             (deserialize pkey (get-store (btree cursor)))))
              (setf (cursor-initialized-p cursor) nil))))
      (cursor-plast cursor)))


;; Duplicated btrees

(defclass rdb-dup-btree (dup-btree rdb-btree) ()
;;  (:metaclass persistent-metaclass)
  (:documentation "A Berkeley Implementation of the duplicate btree"))

(defmethod build-dup-btree ((sc rdb-store))
  (make-instance 'rdb-dup-btree :sc sc))

(defmethod get-value (key (bt rdb-dup-btree))
  (let ((sc (get-store bt)))
    (with-static-streams ((key-buf) (value-buf))
      (write-oid (oid bt) key-buf)
      (ser key key-buf sc)
      (let ((buf (db-get-key-buffered (dup-btrees sc)
                                      key-buf value-buf
                                      :transaction (current-transaction sc))))
        (if buf (values (deserialize buf sc) T)
            (values nil nil))))))

(defmethod existsp (key (bt rdb-dup-btree))
  (let ((sc (get-store bt)))
    (with-static-streams ((key-buf) (value-buf))
      (write-oid (oid bt) key-buf)
      (ser key key-buf sc)
      (let ((buf (db-get-key-buffered 
                  (dup-btrees sc)
                  key-buf value-buf
                  :transaction (current-transaction sc))))
        (if buf t
            nil)))))

;; This is the only difference with the rdb-btree -- I think that means 
;; the other methods can be removed.
(defmethod (setf get-value) (value key (bt rdb-dup-btree))
    (let ((sc (get-store bt)))
      (with-static-streams ((key-buf) (value-buf))
        (write-oid (oid bt) key-buf)
        (ser key key-buf sc)
        (ser value value-buf sc)
        (db-put-buffered (dup-btrees sc)
                         key-buf value-buf
                         :transaction (current-transaction sc)
                         :no-dup t)))
    value)

(defmethod delete-key (key (bt rdb-dup-btree) &key)
  (let ((sc (get-store bt)) )
    (with-static-stream (key-buf)
      (write-oid (oid bt) key-buf)
      (ser key key-buf sc)
      (db-delete-buffered (dup-btrees sc) key-buf 
                          :transaction (current-transaction sc)))))

(defclass rdb-dup-cursor (rdb-cursor) ()
  (:documentation "Cursor for traversing rdb secondary index-table."))

(defmethod make-cursor ((bt rdb-dup-btree))
  "Make a secondary-cursor from a secondary index."
  (let ((sc (get-store bt)))
    (make-instance 'rdb-dup-cursor
                   :btree bt
                   :handle (db-cursor (dup-btrees sc)
                                      :transaction (current-transaction sc))
                   :oid (oid bt))))

(defmethod cursor-next-nodup ((cursor rdb-dup-cursor))
  (if (cursor-initialized-p cursor)
      (with-static-streams ((key-buf) (value-buf))
        (multiple-value-bind (key val)
            (db-cursor-move-buffered (cursor-handle cursor)
                                     key-buf value-buf :next-nodup t)
          (if (and key (= (read-oid key) (cursor-oid cursor)))
              (values t (deserialize key (get-store (btree cursor))) 
                      (deserialize val (get-store (btree cursor))))
              (setf (cursor-initialized-p cursor) nil))))
      (cursor-first cursor)))

(defmethod cursor-delete ((cursor rdb-dup-cursor))
  (if (cursor-initialized-p cursor)
      (progn (db-cursor-delete (cursor-handle cursor))
             (setf (cursor-initialized-p cursor) nil))
      (error "Can't delete with uninitialized cursor!")))

(defmethod open-store ((store rdb-store) &key (recover t)
                                              register
                                              log)
  (if (db-open-p store)
      (progn
        (log:warn! "Database is already open: ~A" store)
        store)
      (with-db (db :db store :open t :close :auto)
        (if (probe-file (name store))
            (progn
              (load-opts db)
              (open-columns* db)
              store)
            store))))

(defmethod close-store ((store rdb-store))
  "Close the underlying RocksDB instance."
  (close-db store))

;; 0-15 reserved cuz why not
(defvar *rdb-oid* 15)
(defvar *rdb-cid* 15)

(defmethod next-oid ((self rdb-store))
  (incf *rdb-oid*))

(defmethod next-cid ((self rdb-store))
  (incf *rdb-cid*))

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
  (< oid 16))

;;; slot protocol
;; TODO 2024-11-07:
(defmethod stored-slot-reader ((self rdb-store) instance name &optional oids-only)
  (declare (ignore oids-only))
  (with-alien ((oid (* unsigned-char) (make-alien unsigned-char 4)))
    (std/alien::write-alien-unsigned-byte-64 oid (the (unsigned-byte 64) (oid instance)))
    (serde (cons name oid) self)
    (let ((ret (get-val (db self) oid)))
      (ensure-transaction (:store self)
        ret))))

(defmethod stored-slot-writer ((self rdb-store) new-value instance name)
  (ensure-transaction (:store self)))

(defmethod stored-slot-boundp ((self rdb-store) instance name)
  (ensure-transaction (:store self)))

(defmethod stored-slot-makunbound ((self rdb-store) instance name)
  (ensure-transaction (:store self)))

;;; Transactions
(defmethod execute-transaction ((self rdb-store) txn
                                &key
                                transaction parent))
