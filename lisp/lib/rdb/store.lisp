;;; store.lisp --- RocksDB Store

;; OBJ/STORE implementation for RocksDB

;;; Code:
(in-package :rdb)

(defclass rdb-btree (btree) ()
  (:documentation "A RocksDB implementation of a BTree."))

(defclass rdb-store (store rdb-database)
  ((btrees :type (or null vector) :accessor btrees)
   (oid-db :type (or null rdb) :accessor oid-db)
   (oid-seq :accessor oid-seq)
   (cid-seq :accessor cid-seq)))

(defmethod build-btree ((st rdb-store))
  (make-instance 'rdb-btree :store st))

(defun rdb-store-spec-p (spec)
  (and (eq (first spec) :rdb)
       (typecase (second spec)
         (pathname t)
         (string t)
         (t nil))))

(defmethod get-value (key (bt rdb-btree))
  (let ((sc (get-store bt)))
    (ensure-transaction (:store sc)
      (buffer-write-oid (oid bt) key-buf)
      (serialize key key-buf sc)
      (let ((buf (db-get-key-buffered (btrees sc)
                                      :transaction (current-transaction sc))))
        (if buf (values (deserialize buf sc) T)
            (values nil nil))))))

(defmethod existsp (key (bt rdb-btree))
  (let ((sc (get-store bt)))
    (ensure-transaction (:store sc)
      (buffer-write-oid (oid bt) key-buf)
      (serialize key key-buf sc)
      (let ((buf (db-get-key-buffered 
                  (btrees sc)
                  :transaction (current-transaction sc))))
          (if buf t
              nil)))))


(defmethod (setf get-value) (value key (bt rdb-btree))
    (let ((sc (get-store bt)))
      (ensure-transaction (:store sc)
        (buffer-write-oid (oid bt) key-buf)
        (serialize key key-buf sc)
        (serialize value value-buf sc)
        (db-put-buffered (btrees sc)
                         :transaction (current-transaction sc))))
  value)


(defmethod delete-key (key (bt rdb-btree) &key)
  (let ((sc (get-store bt)) )
    (ensure-transaction (:store sc)
      (buffer-write-oid (oid bt) key-buf)
      (serialize key key-buf sc)
      (db-delete-buffered (btrees sc)
                          :transaction (current-transaction sc)))))

(defmethod optimize-layout ((bt rdb-btree) &key (freelist-only t) (free-space nil) &allow-other-keys)
  (optimize-layout (get-store bt)
                   :start-key (oid bt)
                   :end-key (oid bt)
                   :freelist-only freelist-only
                   :free-space free-space))

(defclass rdb-indexed-btree (indexed-btree rdb-btree)
  ((indices :accessor indices :initarg :indices :initform (make-hash-table))
   (indices-cache :accessor indices-cache :transient t))
  (:metaclass stored-class)
  (:documentation "A RDB-based BTree supports secondary indices."))

(defmethod indices-cache ((instance rdb-indexed-btree))
  ;; Lazily load the indices-cache to avoid bootstrapping issues: If
  ;; we do not lazy-load the indices cache, it we attempt to
  ;; initialize it before the instance-table is available (thus we
  ;; cannot map oids to classes -- deserialize does not really work
  ;; for complex objects).  -- Red Daly 07/10/2010
  (aif (slot-value instance 'indices-cache)
       it
       (setf (indices-cache instance) (indices instance))))

(defmethod shared-initialize :after ((instance rdb-indexed-btree) slot-names
                                     &rest rest)
  (declare (ignore slot-names rest))
  (setf (indices-cache instance) nil))

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
                 (let ((ht (indices bt))
                       (index (build-btree-index sc 
                                                 :primary bt 
                                                 :key-form key-form)))
                   (setf (gethash index-name (indices-cache bt)) index)
                   (setf (gethash index-name ht) index)
                   (setf (indices bt) ht)
                   index))))
          (when populate (populate bt index))
          index)
        (error "Invalid index initargs!"))))

(defmethod populate ((bt rdb-indexed-btree) index)
  (let ((sc (get-store bt)))
    (with-buffer-streams (primary-buf secondary-buf)
      (flet ((index (key skey)
               (buffer-write-oid (oid bt) primary-buf)
               (serialize key primary-buf sc)
               (buffer-write-oid (oid index) secondary-buf)
               (serialize skey secondary-buf sc)
               ;; should silently do nothing if
               ;; the key/value already exists
               (db-put-buffered 
                (indices sc)
                secondary-buf primary-buf
                :transaction (current-transaction sc))
               (reset-buffer-stream primary-buf)
               (reset-buffer-stream secondary-buf)))
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


(defmethod map-indices (fn (bt rdb-indexed-btree))
  (maphash fn (indices-cache bt)))

(defmethod get-index ((bt rdb-indexed-btree) index-name)
  (gethash index-name (indices-cache bt)))

(defmethod remove-index ((bt rdb-indexed-btree) index-name)
  (remhash index-name (indices-cache bt))
  (let ((indices (indices bt)))
    (remhash index-name indices)
    (setf (indices bt) indices)))

(defmethod (setf get-value) (value key (bt rdb-indexed-btree))
  "Set a key / value pair, and update secondary indices."
  (let ((sc (get-store bt)))
    (let ((indices (indices-cache bt)))
      (with-buffer-streams (key-buf value-buf secondary-buf)
        (buffer-write-oid (oid bt) key-buf)
        (serialize key key-buf sc)
        (serialize value value-buf sc)
        (ensure-transaction (:store sc)
          (db-put-buffered (btrees sc)
                           key-buf value-buf
                           :transaction (current-transaction sc))
          ;; Manually write value into secondary index
          (loop for index being the hash-value of indices
             do
             (multiple-value-bind (index? secondary-key)
                 (funcall (key-fn index) index key value)
               (when index?
                 ;; Insert
                 (buffer-write-oid (oid index) secondary-buf)
                 (serialize secondary-key secondary-buf sc)
                 (db-put-buffered (indices sc)
                                  secondary-buf key-buf
                                  :no-dup t
                                  :transaction (current-transaction sc))
                 (reset-buffer-stream secondary-buf))))
          value)))))

(defmethod delete-key (key (bt rdb-indexed-btree) &key)
  "Remove a key / value pair, and update secondary indices."
  (let ((sc (get-store bt)))
      (with-buffer-streams (key-buf secondary-buf)
        (buffer-write-oid (oid bt) key-buf)
        (serialize key key-buf sc)
        (ensure-transaction (:store sc)
          (let ((value (get-value key bt)))
            (when value
              (let ((indices (indices-cache bt)))
                (loop 
                   for index being the hash-value of indices
                   do
                   (multiple-value-bind (index? secondary-key)
                       (funcall (key-fn index) index key value)
                     (when index?
                       (buffer-write-oid (oid index) secondary-buf)
                       (serialize secondary-key secondary-buf sc)
                       ;; need to remove kv pairs with a cursor! --
                       ;; this is a C performance hack
                       (db-delete-kv-buffered 
                        (indices (get-store bt))
                        secondary-buf key-buf
                        :transaction (current-transaction sc))
                       (reset-buffer-stream secondary-buf))))
                (db-delete-buffered (btrees (get-store bt))
                                    key-buf
                                    :transaction (current-transaction sc)))))))))

;; This also needs to build the correct kind of index, and 
;; be the correct kind of btree...

(defclass rdb-btree-index (btree-index rdb-btree)
  ()
  (:metaclass stored-class)
  (:documentation "A RDB-based BTree supports secondary indices."))

(defmethod get-value (key (bt rdb-btree-index))
  "Get the value in the primary DB from a secondary key."
  (let ((sc (get-store bt)))
    (with-buffer-streams (key-buf value-buf)
      (buffer-write-oid (oid bt) key-buf)
      (serialize key key-buf sc)
      (let ((buf (db-get-key-buffered 
                  (indices-assoc sc)
                  key-buf value-buf
                  :transaction (current-transaction sc))))
        (if buf (values (deserialize buf sc) T)
            (values nil nil))))))

(defmethod get-primary-key (key (bt btree-index))
  (let ((sc (get-store bt)))
    (with-buffer-streams (key-buf value-buf)
      (buffer-write-oid (oid bt) key-buf)
      (serialize key key-buf sc)
      (let ((buf (db-get-key-buffered 
                  (indices sc)
                  key-buf value-buf
                  :transaction (current-transaction sc))))
        (if buf 
            (let ((oid (buffer-read-oid buf)))
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
                   :handle (db-cursor (btrees sc)
                                      :transaction (current-transaction sc))
                   :oid (oid bt))))

(defmethod cursor-close ((cursor rdb-cursor))
  (db-cursor-close (cursor-handle cursor))
  (setf (cursor-initialized-p cursor) nil))

(defmethod cursor-duplicate ((cursor rdb-cursor))
  (make-instance (type-of cursor)
                 :initialized-p (cursor-initialized-p cursor)
                 :oid (cursor-oid cursor)
                 :handle (db-cursor-duplicate 
                          (cursor-handle cursor) 
                          :position (cursor-initialized-p cursor))))

(defmethod cursor-current ((cursor rdb-cursor))
  (when (cursor-initialized-p cursor)
    (let ((sc (get-store (cursor-btree cursor))))
      (with-buffer-streams (key-buf value-buf)
        (multiple-value-bind (key val)
            (db-cursor-move-buffered (cursor-handle cursor) key-buf value-buf
                                     :current t)
          (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
              (progn (setf (cursor-initialized-p cursor) t)
                     (values t (deserialize key sc)
                             (deserialize val sc)))
              (setf (cursor-initialized-p cursor) nil)))))))

(defmethod cursor-first ((cursor rdb-cursor))
  (let ((sc (get-store (cursor-btree cursor))))
    (with-buffer-streams (key-buf value-buf)
      (buffer-write-oid (cursor-oid cursor) key-buf)
      (multiple-value-bind (key val)
          (db-cursor-set-buffered (cursor-handle cursor) 
                                  key-buf value-buf :set-range t)
        (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
            (progn (setf (cursor-initialized-p cursor) t)
                   (values t 
                           (deserialize key sc)
                           (deserialize val sc)))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-last ((cursor rdb-cursor))
  "A fast cursor last, but a bit 'hackish' by exploiting oid ordering"
  (let ((sc (get-store (cursor-btree cursor))))
  (with-buffer-streams (key-buf value-buf)
    ;; Go to the first element of the next btree
    (buffer-write-oid (+ (cursor-oid cursor) 1) key-buf)
    (if (db-cursor-set-buffered (cursor-handle cursor)
                                key-buf value-buf :set-range t)
        (progn (reset-buffer-stream key-buf)
               (reset-buffer-stream value-buf)
               (multiple-value-bind (key val)
                   (db-cursor-move-buffered (cursor-handle cursor) 
                                            key-buf value-buf :prev t)
                 (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
                     (progn
                       (setf (cursor-initialized-p cursor) t)
                       (values t (deserialize key sc)
                                 (deserialize val sc)))
                     (setf (cursor-initialized-p cursor) nil))))
        (multiple-value-bind (key val)
            (db-cursor-move-buffered (cursor-handle cursor) key-buf
                                     value-buf :last t)
          (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
              (progn
                (setf (cursor-initialized-p cursor) t)
                (values t (deserialize key sc)
                        (deserialize val sc )))
              (setf (cursor-initialized-p cursor) nil)))))))

(defmethod cursor-next ((cursor rdb-cursor))
  (if (cursor-initialized-p cursor)
      (let ((sc (get-store (cursor-btree cursor))))
        (with-buffer-streams (key-buf value-buf)
          (multiple-value-bind (key val)
              (the (values (or null buffer-stream)
                           (or null buffer-stream))
                (db-cursor-move-buffered (cursor-handle cursor) 
                                         key-buf value-buf :next t))
            (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
                (the (values t t t)
                  (values t (deserialize key sc)
                          (deserialize val sc)))
                (the null (setf (cursor-initialized-p cursor) nil))))))
      (the t (cursor-first cursor))))

(defmethod cursor-prev ((cursor rdb-cursor))
  (if (cursor-initialized-p cursor)
      (let ((sc (get-store (cursor-btree cursor))))
        (with-buffer-streams (key-buf value-buf)
          (multiple-value-bind (key val)
              (db-cursor-move-buffered (cursor-handle cursor)
                                       key-buf value-buf :prev t)
            (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
                (values t (deserialize key sc)
                        (deserialize val sc))
                (setf (cursor-initialized-p cursor) nil)))))
      (cursor-last cursor)))

(defmethod cursor-set ((cursor rdb-cursor) key)
  (let ((sc (get-store (cursor-btree cursor))))
  (with-buffer-streams (key-buf value-buf)
    (buffer-write-oid (cursor-oid cursor) key-buf)
    (serialize key key-buf sc)
    (multiple-value-bind (k val)
        (db-cursor-set-buffered (cursor-handle cursor)
                                key-buf value-buf :set t)
      (if k
          (progn
            (setf (cursor-initialized-p cursor) t)
            (values t key (deserialize val sc)))
          (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-set-range ((cursor rdb-cursor) key)
  (let ((sc (get-store (cursor-btree cursor))))
  (with-buffer-streams (key-buf value-buf)
    (buffer-write-oid (cursor-oid cursor) key-buf)
    (serialize key key-buf sc)
    (multiple-value-bind (k val)
        (db-cursor-set-buffered (cursor-handle cursor)
                                key-buf value-buf :set-range t)
      (if (and k (= (buffer-read-oid k) (cursor-oid cursor)))
          (progn (setf (cursor-initialized-p cursor) t)
                 (values t (deserialize k sc)
                         (deserialize val sc)))
          (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-get-both ((cursor rdb-cursor) key value)
  (let ((sc (get-store (cursor-btree cursor))))
  (with-buffer-streams (key-buf value-buf)
    (buffer-write-oid (cursor-oid cursor) key-buf)
    (serialize key key-buf sc)
    (serialize value value-buf sc)
    (multiple-value-bind (k v)
        (db-cursor-get-both-buffered (cursor-handle cursor)
                                     key-buf value-buf :get-both t)
      (declare (ignore v))
      (if k
          (progn (setf (cursor-initialized-p cursor) t)
                 (values t key value))
          (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-get-both-range ((cursor rdb-cursor) key value)
  (let ((sc (get-store (cursor-btree cursor))))
  (with-buffer-streams (key-buf value-buf)
    (buffer-write-oid (cursor-oid cursor) key-buf)
    (serialize key key-buf sc)
    (serialize value value-buf sc)
    (multiple-value-bind (k v)
        (db-cursor-get-both-buffered (cursor-handle cursor)
                                     key-buf value-buf :get-both-range t)
      (if k
          (progn (setf (cursor-initialized-p cursor) t)
                 (values t key (deserialize v sc)))
          (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-delete ((cursor rdb-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf value-buf)
        (multiple-value-bind (key val)
            (db-cursor-move-buffered (cursor-handle cursor) key-buf value-buf
                                     :current t)
          (declare (ignore val))
          (when (and key (= (buffer-read-oid key) (cursor-oid cursor)))
            ;; in case of a secondary index this should delete everything
            ;; as specified by the RDB docs.
            (delete-key (deserialize key (get-store (cursor-btree cursor)))
                       (cursor-btree cursor)))
          (setf (cursor-initialized-p cursor) nil)))
      (error "Can't delete with uninitialized cursor!")))

(defmethod cursor-put ((cursor rdb-cursor) value &key (key nil key-specified-p))
  "Put by cursor.  Not particularly useful since standard btrees
   don't support duplicates.  Cursor is invalid after a put"
  (if key-specified-p
      (setf (get-value key (cursor-btree cursor)) value)
      (if (cursor-initialized-p cursor)
          (let ((sc (get-con (cursor-btree cursor))))
            (with-buffer-streams (key-buf value-buf)
              (multiple-value-bind (k v)
                  (db-cursor-move-buffered (cursor-handle cursor) key-buf 
                                           value-buf :current t)
                (declare (ignore v))
                (if (and k (= (buffer-read-oid k) (cursor-oid cursor)))
                    (progn
                      (setf (get-value (deserialize k sc) (cursor-btree cursor))
                            value)
                      (reset-buffer-stream key-buf) (reset-buffer-stream value-buf)
                      (multiple-value-bind (k v)
                          (db-cursor-move-buffered (cursor-handle cursor) key-buf
                                                   value-buf :next t)
                        (if (and key (= (buffer-read-oid k) (cursor-oid cursor)))
                            (values t (deserialize k sc) (deserialize v sc))
                            (setf (cursor-initialized-p cursor) nil))))
                    (setf (cursor-initialized-p cursor) nil)))))
          (error "Can't put with uninitialized cursor!"))))

;; Secondary cursors

(defclass rdb-secondary-cursor (secondary-cursor rdb-cursor) ()
  (:documentation "Cursor for traversing rdb secondary indices."))

(defmethod make-cursor ((bt rdb-btree-index))
  "Make a secondary-cursor from a secondary index."
  (let ((sc (get-store bt)))
    (make-instance 'rdb-secondary-cursor 
                   :btree bt
                   :handle (db-cursor (indices-assoc sc)
                                      :transaction (my-current-transaction sc))
                   :oid (oid bt))))

(defmethod cursor-pcurrent ((cursor rdb-secondary-cursor))
  (when (cursor-initialized-p cursor)
    (with-buffer-streams (key-buf pkey-buf value-buf)
      (multiple-value-bind (key pkey val)
          (db-cursor-pmove-buffered (cursor-handle cursor)
                                    key-buf pkey-buf value-buf
                                    :current t)
        (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
            (progn (setf (cursor-initialized-p cursor) t)
                   (let ((sc (get-store (cursor-btree cursor))))
                     (values t 
                             (deserialize key sc)
                             (deserialize val sc)
                             (progn (buffer-read-oid pkey) (deserialize pkey sc)))))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-pfirst ((cursor rdb-secondary-cursor))
  (with-buffer-streams (key-buf pkey-buf value-buf)
    (buffer-write-oid (cursor-oid cursor) key-buf)
    (multiple-value-bind (key pkey val)
        (db-cursor-pset-buffered (cursor-handle cursor) 
                                 key-buf pkey-buf value-buf :set-range t)
      (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
          (progn (setf (cursor-initialized-p cursor) t)
                 (let ((sc (get-con (cursor-btree cursor))))
                 (values t 
                         (deserialize key sc)
                         (deserialize val sc)
                         (progn (buffer-read-oid pkey) (deserialize pkey sc)))))
          (setf (cursor-initialized-p cursor) nil)))))

;;A bit of a hack.....
(defmethod cursor-plast ((cursor rdb-secondary-cursor))
  (let ((sc (get-con (cursor-btree cursor))))
  (with-buffer-streams (key-buf pkey-buf value-buf)
    (buffer-write-oid (+ (cursor-oid cursor) 1) key-buf)
    (if (db-cursor-set-buffered (cursor-handle cursor) 
                                key-buf value-buf :set-range t)    
        (progn (reset-buffer-stream key-buf)
               (reset-buffer-stream value-buf)
               (multiple-value-bind (key pkey val)
                   (db-cursor-pmove-buffered (cursor-handle cursor) key-buf 
                                             pkey-buf value-buf :prev t)
                 (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
                     (progn
                       (setf (cursor-initialized-p cursor) t)
                       (values t 
                               (deserialize key sc)
                               (deserialize val sc)
                               (progn (buffer-read-oid pkey) 
                                      (deserialize pkey sc))))
                     (setf (cursor-initialized-p cursor) nil))))
        (multiple-value-bind (key pkey val)
            (db-cursor-pmove-buffered (cursor-handle cursor) key-buf
                                      pkey-buf value-buf :last t)
          (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
              (progn
                (setf (cursor-initialized-p cursor) t)
                (values t (deserialize key sc)
                        (deserialize val sc)
                        (progn (buffer-read-oid pkey) (deserialize pkey sc))))
              (setf (cursor-initialized-p cursor) nil)))))))

(defmethod cursor-pnext ((cursor rdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf pkey-buf value-buf)
        (multiple-value-bind (key pkey val)
            (db-cursor-pmove-buffered (cursor-handle cursor) 
                                     key-buf pkey-buf value-buf :next t)
          (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
              (let ((sc (get-con (cursor-btree cursor))))
                (values t (deserialize key sc)
                        (deserialize val sc)
                        (progn (buffer-read-oid pkey) (deserialize pkey sc))))
              (setf (cursor-initialized-p cursor) nil))))
      (cursor-pfirst cursor)))

(defmethod cursor-pprev ((cursor rdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf pkey-buf value-buf)
        (multiple-value-bind (key pkey val)
            (db-cursor-pmove-buffered (cursor-handle cursor)
                                      key-buf pkey-buf value-buf :prev t)
          (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
              (let ((sc (get-con (cursor-btree cursor))))
                (values t (deserialize key sc)
                        (deserialize val sc)
                        (progn (buffer-read-oid pkey) (deserialize pkey sc))))
              (setf (cursor-initialized-p cursor) nil))))
      (cursor-plast cursor)))

(defmethod cursor-pset ((cursor rdb-secondary-cursor) key)
  (let ((sc (get-con (cursor-btree cursor))))
    (with-buffer-streams (key-buf pkey-buf value-buf)
      (buffer-write-oid (cursor-oid cursor) key-buf)
      (serialize key key-buf sc)
      (multiple-value-bind (k pkey val)
          (db-cursor-pset-buffered (cursor-handle cursor)
                                   key-buf pkey-buf value-buf :set t)
        (if k
            (progn
              (setf (cursor-initialized-p cursor) t)
              (values t key (deserialize val sc)
                      (progn (buffer-read-oid pkey) 
                             (deserialize pkey sc))))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-pset-range ((cursor rdb-secondary-cursor) key)
  (let ((sc (get-con (cursor-btree cursor))))
    (with-buffer-streams (key-buf pkey-buf value-buf)
      (buffer-write-oid (cursor-oid cursor) key-buf)
      (serialize key key-buf sc)
      (multiple-value-bind (k pkey val)
          (db-cursor-pset-buffered (cursor-handle cursor)
                                   key-buf pkey-buf value-buf :set-range t)
        (if (and k (= (buffer-read-oid k) (cursor-oid cursor)))
            (progn (setf (cursor-initialized-p cursor) t)
                   (values t (deserialize k sc)
                           (deserialize val sc)
                           (progn (buffer-read-oid pkey) (deserialize pkey sc))))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-pget-both ((cursor rdb-secondary-cursor) key pkey)
  (with-buffer-streams (key-buf pkey-buf value-buf)
    (let ((primary-oid (oid (primary (cursor-btree cursor))))
          (sc (get-con (cursor-btree cursor))))
      (buffer-write-oid (cursor-oid cursor) key-buf)
      (serialize key key-buf sc)
      (buffer-write-oid primary-oid pkey-buf)
      (serialize pkey pkey-buf sc)
      (multiple-value-bind (k p val)
          (db-cursor-pget-both-buffered (cursor-handle cursor)
                                        key-buf pkey-buf value-buf :get-both t)
        (declare (ignore p))
        (if k
            (progn (setf (cursor-initialized-p cursor) t)
                   (values t key (deserialize val sc) pkey))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-pget-both-range ((cursor rdb-secondary-cursor) key pkey)
  (with-buffer-streams (key-buf pkey-buf value-buf)
    (let ((primary-oid (oid (primary (cursor-btree cursor))))
          (sc (get-con (cursor-btree cursor))))
      (buffer-write-oid (cursor-oid cursor) key-buf)
      (serialize key key-buf sc)
      (buffer-write-oid primary-oid pkey-buf)
      (serialize pkey pkey-buf sc)
      (multiple-value-bind (k p val)
          (db-cursor-pget-both-buffered (cursor-handle cursor) key-buf 
                                        pkey-buf value-buf :get-both-range t)
        (if k
            (progn (setf (cursor-initialized-p cursor) t)
                   (values t key (deserialize val sc)
                           (progn (buffer-read-oid p) (deserialize p sc))))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-delete ((cursor rdb-secondary-cursor))
  "Delete by cursor: deletes ALL secondary index values."
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf pkey-buf value-buf)
        (multiple-value-bind (key pkey val)
            (db-cursor-pmove-buffered (cursor-handle cursor) key-buf pkey-buf
                                      value-buf :current t)
          (declare (ignore val))
          (when (and key (= (buffer-read-oid key) (cursor-oid cursor))
                     (= (buffer-read-oid pkey) (oid (primary
                                                     (cursor-btree cursor)))))
            (delete-key (deserialize pkey (get-con (cursor-btree cursor)))
                       (primary (cursor-btree cursor))))
          (setf (cursor-initialized-p cursor) nil)))
      (error "Can't delete with uninitialized cursor!")))

(defmethod cursor-next-dup ((cursor rdb-secondary-cursor))
  (when (cursor-initialized-p cursor)
    (with-buffer-streams (key-buf value-buf)
      (multiple-value-bind (key val)
          (db-cursor-move-buffered (cursor-handle cursor)
                                   key-buf value-buf :next-dup t)
        (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
            (values t (deserialize key (get-con (cursor-btree cursor))) 
                    (deserialize val (get-con (cursor-btree cursor))))
            (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-next-nodup ((cursor rdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf value-buf)
        (multiple-value-bind (key val)
            (db-cursor-move-buffered (cursor-handle cursor)
                                     key-buf value-buf :next-nodup t)
          (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
              (values t (deserialize key (get-con (cursor-btree cursor))) 
                      (deserialize val (get-con (cursor-btree cursor))))
              (setf (cursor-initialized-p cursor) nil))))
      (cursor-first cursor)))

(defmethod cursor-prev-nodup ((cursor rdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf value-buf)
        (multiple-value-bind (key val)
          (the (values (or null buffer-stream) 
                       (or null buffer-stream))
            (db-cursor-move-buffered (cursor-handle cursor)
                                     key-buf value-buf :prev-nodup t))
          (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
              (values t (deserialize key (get-con (cursor-btree cursor))) 
                      (deserialize val (get-con (cursor-btree cursor))))
              (setf (cursor-initialized-p cursor) nil))))
      (cursor-last cursor)))

(defmethod cursor-pnext-dup ((cursor rdb-secondary-cursor))
  (when (cursor-initialized-p cursor)
    (with-buffer-streams (key-buf pkey-buf value-buf)
      (multiple-value-bind (key pkey val)
          (the (values (or null buffer-stream) 
                       (or null buffer-stream)
                       (or null buffer-stream))
            (db-cursor-pmove-buffered (cursor-handle cursor)
                                    key-buf pkey-buf value-buf :next-dup t))
        (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
            (the (values t t t t)
              (values t (deserialize key (get-con (cursor-btree cursor)))
                      (deserialize val (get-con (cursor-btree cursor)))
                      (progn (buffer-read-oid pkey) (deserialize pkey (get-con (cursor-btree cursor))))))
            (the null (setf (cursor-initialized-p cursor) nil)))))))

(defmethod cursor-pnext-nodup ((cursor rdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf pkey-buf value-buf)
        (multiple-value-bind (key pkey val)
            (db-cursor-pmove-buffered (cursor-handle cursor) key-buf
                                      pkey-buf value-buf :next-nodup t)
          (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
              (values t (deserialize key (get-con (cursor-btree cursor))) 
                      (deserialize val (get-con (cursor-btree cursor)))
                      (progn (buffer-read-oid pkey) (deserialize pkey (get-con (cursor-btree cursor)))))
              (setf (cursor-initialized-p cursor) nil))))
      (cursor-pfirst cursor)))

(defmethod cursor-pprev-nodup ((cursor rdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf pkey-buf value-buf)
        (multiple-value-bind (key pkey val)
            (db-cursor-pmove-buffered (cursor-handle cursor) key-buf
                                      pkey-buf value-buf :prev-nodup t)
          (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
              (values t (deserialize key (get-con (cursor-btree cursor)))
                      (deserialize val (get-con (cursor-btree cursor)))
                      (progn (buffer-read-oid pkey)
                             (deserialize pkey (get-con (cursor-btree cursor)))))
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
    (with-buffer-streams (key-buf value-buf)
      (buffer-write-oid (oid bt) key-buf)
      (serialize key key-buf sc)
      (let ((buf (db-get-key-buffered (dup-btrees sc)
                                      key-buf value-buf
                                      :transaction (current-transaction sc))))
        (if buf (values (deserialize buf sc) T)
            (values nil nil))))))

(defmethod existsp (key (bt rdb-dup-btree))
  (let ((sc (get-store bt)))
    (with-buffer-streams (key-buf value-buf)
      (buffer-write-oid (oid bt) key-buf)
      (serialize key key-buf sc)
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
      (with-buffer-streams (key-buf value-buf)
        (buffer-write-oid (oid bt) key-buf)
        (serialize key key-buf sc)
        (serialize value value-buf sc)
        (db-put-buffered (dup-btrees sc)
                         key-buf value-buf
                         :transaction (my-current-transaction sc)
                         :no-dup t)))
    value)

(defmethod delete-key (key (bt rdb-dup-btree) &key)
  (let ((sc (get-store bt)) )
    (with-buffer-streams (key-buf)
      (buffer-write-oid (oid bt) key-buf)
      (serialize key key-buf sc)
      (db-delete-buffered (dup-btrees sc) key-buf 
                          :transaction (current-transaction sc)))))

(defclass rdb-dup-cursor (rdb-cursor) ()
  (:documentation "Cursor for traversing rdb secondary indices."))

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
      (with-buffer-streams (key-buf value-buf)
        (multiple-value-bind (key val)
            (db-cursor-move-buffered (cursor-handle cursor)
                                     key-buf value-buf :next-nodup t)
          (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
              (values t (deserialize key (get-con (cursor-btree cursor))) 
                      (deserialize val (get-con (cursor-btree cursor))))
              (setf (cursor-initialized-p cursor) nil))))
      (cursor-first cursor)))

(defmethod cursor-delete ((cursor rdb-dup-cursor))
  (if (cursor-initialized-p cursor)
      (progn (db-cursor-delete (cursor-handle cursor))
             (setf (cursor-initialized-p cursor) nil))
      (error "Can't delete with uninitialized cursor!")))

(defmethod open-store ((store rdb-store) &key (recover t)
                                              register
                                              log))

(defmethod close-store ((store rdb-store)))

(defmethod next-oid ((self rdb-store)))
(defmethod next-cid ((self rdb-store)))

(defmethod oid->schema-id (oid (sc rdb-store))
  "For default data structures, provide a fixed mapping to class IDs based
   on the known startup order.  It's ugly, it's sad, but it works."
  (if (< oid 2)
      (case oid
        (0 4)
        (1 4)
        (-1 1)
        (-2 1)
        (-3 3)
        (-4 3))
      (call-next-method)))

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

;; db version
(defmethod database-version ((sc rdb-store))
  "Elephant protocol to provide the version tag or nil if unmarked"
  (with-buffer-streams (key val)
    (serialize-database-version-key key)
    (let ((buf (db-get-key-buffered (store-metadata sc)
                                    key val
                                    :transaction +NULL-VOID+)))
      (if buf (deserialize-database-version-value buf)
          nil))))

;;; slot protocol
;; TODO 2024-11-07: 
(defmethod stored-slot-reader ((self rdb-store) instance name &optional oids-only)
  (declare (ignore oids-only))
  (ensure-transaction (:store self)))

(defmethod stored-slot-writer ((self rdb-store) new-value instance name)
  (ensure-transaction (:store self)))

(defmethod stored-slot-boundp ((self rdb-store) instance name)
  (ensure-transaction (:store self)))

(defmethod stored-slot-makunbound ((self rdb-store) instance name)
  (ensure-transaction (:store self)))

;;; transaction protocol
(defmethod execute-transaction ((self rdb-store) txn
                                &key
                                transaction parent))

(defmethod start-transaction ((self rdb-store) transaction &key))

(defmethod commit-transaction (store transaction &key)
  (assert (not *txn*))
  ;; TODO 2024-11-07: 
  )

(defmethod abort-transaction (self transaction &key))
