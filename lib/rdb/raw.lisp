;;; rdb/raw.lisp --- Raw API

;; RocksDB functions which support octet-vectors and strings only.

;;; Code:
(in-package :rdb)

;; the may-exist-p functions are part of the raw API - buffer API returns a
;; single value (null or a stream).
(defun %key-may-exist-p (db key length &optional (opts (rocksdb-readoptions-create)) timestamp)
  (with-alien ((found boolean)
               (v (* unsigned-char))
               (vlen size-t))
    (and
     (rocksdb-key-may-exist db opts key length (addr v) (addr vlen) 
                            timestamp (if timestamp (length timestamp) 0)
                            (addr found))
     found
     (not (zerop vlen))
     (values v vlen))))

(defun %cf-key-may-exist-p (db cf key length &optional (opts (rocksdb-readoptions-create)) timestamp)
  (with-alien ((found boolean)
               (v (* unsigned-char))
               (vlen size-t))
    (and
     (rocksdb-key-may-exist-cf db opts cf key length (addr v) (addr vlen) 
                               timestamp (if timestamp (length timestamp) 0)
                               (addr found))
     found
     (not (zerop vlen))
     (values v vlen))))

;; TODO 2026-08-20: 
#+nil
(defmacro unless-key-exists-p ((key length db &key cf (options (default-rocksdb-readoptions)) timestamp) &body body)
  "If KEY of given LENGTH _might_ exist (probabilistic) in DB (or CF) do nothing,
else eval forms in BODY.

This does not necessarily guarantee KEY does not exist before using
[[id:OBJ/DB:PUT-KEY][put-key]]. An alternative approach would be to use a custom merge-operator which
does nothing when merging with an existing key."
  (with-gensyms (v vlen)
    `(multiple-value-bind (,v ,vlen) ,(if cf 
                                          `(%cf-key-may-exist-p ,db ,cf ,key ,length ,options ,timestamp)
                                          `(%key-may-exist-p ,db ,key ,length ,options ,timestamp))
       (declare (ignorable ,vlen))
       (if ,v
           (rocksdb-free ,v)
           (progn
             ,@body)))))

(defun %put-kv (db key val &optional (opts (rocksdb-writeoptions-create)))
    (with-kv-raw (db key e :error put-kv-error :val val)
      (rocksdb-put db opts
           %key %klen
           %val %vlen
           e)))

(defun %put-kv-str (db key val &optional (opts (rocksdb-writeoptions-create)))
  (let ((key-octets (string-to-octets key :null-terminate nil))
        (val-octets (string-to-octets val :null-terminate nil)))
    (%put-kv db key-octets val-octets opts)))

(defun %get-kv (db key &optional (opt (rocksdb-readoptions-create)) pinned)
  (with-kv-raw (db key e :error get-kv-error)
    (if pinned 
        (rocksdb-get-pinned db opt %key %klen e)
        (multiple-value-bind (val vlen)
            (rocksdb-get db
                         opt
                         %key
                         %klen
                         e)
          (let ((v (make-octets vlen)))
            (clone-octets-from-alien val v vlen)
            (coerce v 'octet-vector))))))

(defun %get-kv-str (db key &optional (opt (rocksdb-readoptions-create)) pinned)
  (let ((k (string-to-octets key)))
    (let ((v (%get-kv db k opt pinned)))
      (when v (octets-to-string v)))))

(defun %multi-get-kv (db keys &optional (opt (rocksdb-readoptions-create)))
  (multiple-value-bind (keys keyns) (clone-octet-vector-list* keys)
    (let ((n (length keys)))
      (with-alien ((vals (* (* (unsigned 8))) (make-alien (* (unsigned 8)) n))
                   (valns (* size-t) (make-alien size-t n))
                   (errs (* c-string) (make-alien c-string n)))  
        (rocksdb-multi-get db opt n keys keyns vals valns errs)))))

(defun %multi-get-kv-str (db keys &optional (opt (rocksdb-readoptions-create)))
  (let ((n (length keys))
        (keys (clone-strings keys nil))
        (keyns (clone-integer-list (mapcar 'length keys))))
    (with-alien ((vals (* (* (unsigned 8))) (make-alien (* (unsigned 8)) n))
                 (valns (* size-t) (make-alien size-t n))
                 (errs (* c-string) (make-alien c-string n)))
      (rocksdb-multi-get db opt n keys keyns vals valns errs))))

(defun %multi-get-cf-kv (db cfs keys &optional (opt (rocksdb-readoptions-create)))
  (multiple-value-bind (keys keyns) (clone-octet-vector-list* keys)
    (let ((n (length keys)))
      (with-alien ((%cfs (* (* rocksdb-column-family-handle)) (make-alien (* rocksdb-column-family-handle) n))
                   (vals (* (* (unsigned 8))) (make-alien (* (unsigned 8)) n))
                   (valns (* size-t) (make-alien size-t n))
                   (errs (* c-string) (make-alien c-string n)))
        (loop for i below n do (setf (deref %cfs i) (pop cfs)))
        (rocksdb-multi-get-cf db opt cfs n keys keyns vals valns errs)))))

(defun %multi-get-cf-kv-str (db cfs keys &optional (opt (rocksdb-readoptions-create)))
  (let ((n (length keys))
        (keys (clone-strings keys nil))
        (keyns (clone-integer-list (mapcar 'length keys))))
    (with-alien ((%cfs (* (* rocksdb-column-family-handle)) (make-alien (* rocksdb-column-family-handle) n))
                 (vals (* (* (unsigned 8))) (make-alien (* (unsigned 8)) n))
                 (valns (* size-t) (make-alien size-t n))
                 (errs (* c-string) (make-alien c-string n)))
      (loop for i below n do (setf (deref %cfs i) (pop cfs)))
      (rocksdb-multi-get-cf db opt cfs n keys keyns vals valns errs))))

(defun %merge-kv (db key val &optional (opt (rocksdb-writeoptions-create)))
  (with-kv-raw (db key e :error merge-kv-error :val val)
    (rocksdb-merge db opt %key %klen %val %vlen e)))

(defun %merge-kv-str (db key val &optional (opt (rocksdb-writeoptions-create)))
  (let ((k (string-to-octets key))
        (v (string-to-octets val)))
    (%merge-kv db k v opt)))

(defun %delete-kv (db key &optional (opt (rocksdb-writeoptions-create)))
  (with-kv-raw (db key e)
    (rocksdb-delete db opt %key %klen e)))

(defun %delete-kv-str (db key &optional (opt (rocksdb-writeoptions-create)))
  (let ((k (string-to-octets key)))
    (%delete-kv db k opt)))

(defun %get-cf (db cf key &optional (opt (rocksdb-readoptions-create)) pinned)
  (with-kv-raw (db key e :error get-kv-cf-error :cf cf)
      (with-alien ((vlen (* size-t) (make-alien size-t)))
        (let ((val (if pinned
                       (rocksdb-get-pinned db opt %key %klen e)
                       (rocksdb-get-cf 
                        db
                        opt
                        cf
                        %key 
                        %klen
                        vlen
                        e)))
          ;; helps if we know the vlen beforehand, would need a custom
          ;; C-side function probably.
          (v (make-array (deref vlen) :element-type 'octet)))
          (let ((ret (clone-octets-from-alien val v (deref vlen))))
            (unless (zerop (length ret))
              ret))))))

(defun %get-cf-str (db cf key &optional (opt (rocksdb-readoptions-create)) pinned)
  (let ((k (string-to-octets key :null-terminate nil)))
    (let ((v (%get-cf db cf k opt pinned)))
      (when v (octets-to-string v)))))

(defun %put-cf (db cf key val &optional (opts (rocksdb-writeoptions-create)))
  (with-kv-raw (db key e :error put-kv-cf-error :val val :cf cf)
    (rocksdb-put-cf db
                    opts
                    cf
                    %key %klen
                    %val %vlen
                    e)))

(defun %put-cf-str (db cf key val &optional (opt (rocksdb-writeoptions-create)))
  (let ((key-octets (string-to-octets key :null-terminate nil))
        (val-octets (string-to-octets val :null-terminate nil)))
    (%put-cf db cf key-octets val-octets opt)))

(defun %merge-cf (db cf key val &optional (opt (rocksdb-writeoptions-create)))
  (with-kv-raw (db key e :cf cf :error merge-kv-cf-error :val val)
    (rocksdb-merge-cf db opt cf %key %klen %val %vlen e)))

(defun %merge-cf-str (db cf key val &optional (opt (rocksdb-writeoptions-create)))
  (let ((k (string-to-octets key))
        (v (string-to-octets val)))
    (%merge-cf db cf k v opt)))

(defun %iter-key (iter)
  (multiple-value-bind (key klen) (rocksdb-iter-key iter)
    (let ((k (make-array klen :element-type '(unsigned-byte 8))))
      (clone-octets-from-alien key k klen)
      k)))

(defun %iter-key-str (iter)
  (when-let ((k (%iter-key iter)))
    (octets-to-string k)))

(defun %iter-val (iter)
  (multiple-value-bind (val vlen) (rocksdb-iter-value iter)
    (let ((v (make-array vlen :element-type '(unsigned-byte 8))))
      (clone-octets-from-alien val v vlen)
      v)))

(defun %iter-valid-p (iter)
  (rocksdb-iter-valid iter))

(defun %iter-val-str (iter)
  (when-let ((v (%iter-val iter)))
    (octets-to-string v)))

;; this function is deprecated in the Java API:
;; https://javadoc.io/doc/org.rocksdb/rocksdbjni/6.6.4/org/rocksdb/SstFileWriter.html
;; (defun %sst-add (writer key val)
;;   (with-errptr* (err 'rdb-alien-error)
;;     (rocksdb-sstfilewriter-add writer key (length key) val (length val) err)))

(defun %sst-put (writer key val)
  (let ((klen (length key))
        (vlen (length val)))
    (with-errptr* (err 'rdb-alien-error)
      (with-alien ((k (* unsigned-char) (make-alien unsigned-char klen))
                   (v (* unsigned-char) (make-alien unsigned-char vlen)))
        (setfa k key)
        (setfa v val)
        (rocksdb-sstfilewriter-put writer k klen v vlen err)))))

(defun %sst-put-str (writer key val)
  (let ((key-octets (string-to-octets key :null-terminate nil))
        (val-octets (string-to-octets val :null-terminate nil)))
    (%sst-put writer key-octets val-octets)))

(defun %sst-put-ts (writer key val ts)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-sstfilewriter-put-with-ts writer key (length key) val (length val) ts (length ts) err)))

(defun %sst-delete (writer key)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-sstfilewriter-delete writer key (length key) err)))

(defun %sst-delete-ts (writer key ts)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-sstfilewriter-delete-with-ts writer key (length key) ts (length ts) err)))

(defun %sst-delete-range (writer start-key end-key)
  (with-errptr* (err 'rdb-alien-error)
    (rocksdb-sstfilewriter-delete-range writer start-key (length start-key) end-key (length end-key) err)))

(defun %transactiondb-get-kv (db key &optional (opts (rocksdb-readoptions-create)) pinned)
  (with-kv-raw (db key e :error get-kv-error)
    (with-alien ((vlen size-t))
      (let* ((val (if pinned
                      (rocksdb-transactiondb-get-pinned db opts %key %klen e)
                      (rocksdb-transactiondb-get db opts %key %klen vlen e)))
             (v (make-array vlen :element-type 'octet)))
        (clone-octets-from-alien val v vlen)
        v))))

(defun %transactiondb-get-kv-str (db key &optional (opts (rocksdb-readoptions-create)) pinned)
  (let ((k (string-to-octets key)))
    (let ((v (%transactiondb-get-kv db k opts pinned)))
      (when v (octets-to-string v)))))

(defun %transactiondb-get-cf (db cf key &optional (opts (rocksdb-readoptions-create)) pinned)
  (with-kv-raw (db key e :error get-kv-cf-error :cf cf)
    (if pinned
        (rocksdb-transactiondb-get-pinned-cf db opts cf %key %klen e)
        (multiple-value-bind (val vlen)
            (rocksdb-transactiondb-get-cf db opts cf %key %klen e)
          (let ((v (make-array vlen :element-type 'octet)))
            (clone-octets-from-alien val v vlen)
            v)))))

(defun %transactiondb-get-cf-str (db cf key &optional (opts (rocksdb-readoptions-create)) pinned)
  (let ((k (string-to-octets key)))
    (let ((v (%transactiondb-get-cf db cf k opts pinned)))
      (when v (octets-to-string v)))))

(defun %transactiondb-put-kv (db key val &optional (opts (rocksdb-writeoptions-create)))
  (with-kv-raw (db key e :error put-kv-error :val val)
    (rocksdb-transactiondb-put db opts %key %klen %val %vlen e)))

(defun %transactiondb-put-kv-str (db key val &optional (opts (rocksdb-writeoptions-create)))
  (let ((key-octets (string-to-octets key :null-terminate nil))
        (val-octets (string-to-octets val :null-terminate nil)))
    (%transactiondb-put-kv db key-octets val-octets opts)))

(defun %transactiondb-put-cf-kv (db cf key val &optional (opts (rocksdb-writeoptions-create)))
  (with-kv-raw (db key e :error put-kv-cf-error :val val :cf cf)
    (rocksdb-transactiondb-put-cf db
                                  opts
                                  cf
                                  %key %klen
                                  %val %vlen
                                  e)))

(defun %transactiondb-put-cf-str (db cf key val &optional (opts (rocksdb-writeoptions-create)))
  (let ((key-octets (string-to-octets key :null-terminate nil))
        (val-octets (string-to-octets val :null-terminate nil)))
    (%transactiondb-put-cf-kv db cf key-octets val-octets opts)))

(defun %transaction-name (txn)
  (multiple-value-bind (name len) (rocksdb-transaction-get-name txn)
    (lety ((buf (make-string len) :type simple-string))
      (loop for i from 0 below len
            do (setf (aref buf i) (code-char (deref name i))))
      buf)))

(defun %set-transaction-name (txn name)
  (with-errptr* (e 'rdb-transaction-error :txn txn)
    (let ((nlen (length name)))
      (with-alien ((%name (* unsigned-char) (octets-to-alien (string-to-octets name))))
        (rocksdb-transaction-set-name txn %name nlen e)))))

(defsetf %transaction-name %set-transaction-name)

(defun %transaction-get (txn key &optional (opts (rocksdb-readoptions-create)) pinned)
  (with-txn-raw (txn e :key key)
    (if pinned
        (rocksdb-transaction-get-pinned txn opts %key %klen e)
        (rocksdb-transaction-get txn opts %key %klen e))))

(defun %transaction-get-cf (txn cf key &optional (opts (rocksdb-readoptions-create)) pinned)
  (with-txn-raw (txn e :key key :cf cf)
    (if pinned
        (rocksdb-transaction-get-pinned-cf txn opts cf %key %klen e)
        (rocksdb-transaction-get-cf txn opts cf %key %klen e))))

(defun %transaction-delete (txn key)
  (with-txn-raw (txn e :key key)
    (rocksdb-transaction-delete txn %key %klen e)))

(defun %transaction-delete-cf (txn cf key)
  (with-txn-raw (txn e :key key :cf cf)
    (rocksdb-transaction-delete-cf txn cf %key %klen e)))

(defun %transaction-put (txn key val)
  (with-txn-raw (txn e :key key :val val)
    (rocksdb-transaction-put txn %key %klen %val %vlen e)))

(defun %transaction-put-cf (txn cf key val)
  (with-txn-raw (txn e :cf cf :key key :val val)
    (rocksdb-transaction-put-cf txn cf %key %klen %val %vlen e)))

(defun %transaction-merge (txn key val)
  (with-txn-raw (txn e :key key :val val)
    (rocksdb-transaction-merge txn %key %klen %val %vlen e)))

(defun %transaction-merge-cf (txn cf key val)
  (with-txn-raw (txn e :key key :val val :cf cf)
    (rocksdb-transaction-merge-cf txn cf %key %klen %val %vlen e)))

(defun %wbwi-data (wbwi)
  (multiple-value-bind (data size) (rocksdb-writebatch-wi-data wbwi)
    (clone-octets-from-alien data (make-array size :element-type 'octet))))
(defun %writebatch-data (wb)
  (multiple-value-bind (data size) (rocksdb-writebatch-data wb)
    (clone-octets-from-alien data (make-array size :element-type 'octet))))
(defun %wbwi-ts (self ts)
  (with-errptr e
    (rocksdb-writebatch-wi-update-timestamps 
     self (octets-to-alien ts) (length ts) nil nil e)))

(defun %wbwi-put-cf (wbwi cf key val)
  (with-kv-raw* key val
    (rocksdb-writebatch-wi-put-cf 
     wbwi
     cf
     %key %klen
     %val %vlen)))
(defun %wbwi-put-kv (self key val)
  (declare (octet-vector key val))
  (rocksdb-writebatch-wi-put 
   self
   (cast (octets-to-alien key) (array unsigned-char))
   (length key) 
   (cast (octets-to-alien val) (array unsigned-char))
   (length val)))

(defun %wbwi-put-kv-str (self key val)
  (%wbwi-put-kv self (string-to-octets key) (string-to-octets val)))

(defun %wbwi-kv (self key &optional (opt (rocksdb-readoptions-create)))
  (with-errptr e
    (multiple-value-bind (data i)
        (rocksdb-writebatch-wi-get-from-batch
         self
         opt
         (cast (octets-to-alien key) (array unsigned-char))
         (length key)
         e)
      (std:clone-octets-from-alien 
       data
       (make-array i :element-type 'octet)))))

(defun %wbwi-kv-str (self key &optional (opt (rocksdb-readoptions-create)))
  (let ((k (string-to-octets key)))
    (let ((v (%wbwi-kv self k opt)))
      (when v (octets-to-string v)))))

;;; zero-copy
(defun %get-kv-pinned (db key &optional (opt (rocksdb-readoptions-create)))
  "DB get using the v2 zero-copy API."
  (with-kv-raw (db key e :error get-kv-error)
    (rocksdb-get-pinned-v2 db opt %key %klen e)))

(defun %get-kv-cf-pinned (db key cf &optional (opt (rocksdb-readoptions-create)))
  "DB get CF using the v2 zero-copy API."
  (with-kv-raw (db key e :error get-kv-cf-error :cf cf)
    (rocksdb-get-pinned-cf-v2 db opt cf %key %klen e)))

(defun %get-kv-buffer (db key buffer &optional (opt (rocksdb-readoptions-create)))
  "DB get using the 'into_buffer' API."
  (with-kv-raw (db key e :error get-kv-error)
    (rocksdb-get-into-buffer db opt %key %klen buffer (length buffer) e)))

(defun %get-kv-cf-buffer (db key cf buffer &optional (opt (rocksdb-readoptions-create)))
  "DB get CF using the 'into_buffer' API."
  (with-kv-raw (db key e :error get-kv-cf-error :cf cf)
    (rocksdb-get-into-buffer-cf db opt cf %key %klen buffer (length buffer) e)))
