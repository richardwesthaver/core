(in-package :rocksdb)

;;; DB
(def-with-errptr rocksdb-open (* rocksdb)
  (opt (* rocksdb-options))
  (name c-string))

(define-alien-routine rocksdb-close void 
      (db (* rocksdb)))

(define-alien-routine rocksdb-cancel-all-background-work void 
  (db (* rocksdb))
  (wait boolean))

(export '(rocksdb-close rocksdb-cancel-all-background-work))

(def-with-errptr rocksdb-put 
  void 
  (db (* rocksdb))
  (options (* rocksdb-writeoptions))
  (key (* char))
  (keylen size-t) 
  (val (* char))
  (vallen size-t))

(def-with-errptr rocksdb-get 
  (* char)
  (db (* rocksdb))
  (options (* rocksdb-readoptions))
  (key (* char))
  (keylen size-t) 
  (vallen (* size-t)))

(def-with-errptr rocksdb-delete 
  void
  (db (* rocksdb))
  (options (* rocksdb-writeoptions))
  (key (* char))
  (keylen size-t))

(def-with-errptr rocksdb-merge 
  void
  (db (* rocksdb))
  (opt (* rocksdb-writeoptions))
  (key (* char))
  (keylen size-t)
  (val (* char))
  (vallen size-t))

(def-with-errptr rocksdb-merge-cf 
  void
  (db (* rocksdb))
  (opt (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* char))
  (keylen size-t)
  (val (* char))
  (vallen size-t))

(def-with-errptr rocksdb-write 
  void
  (db (* rocksdb))
  (opt (* rocksdb-writeoptions))
  (batch (* rocksdb-writebatch)))

(def-with-errptr rocksdb-get-cf 
  (* char)
  (db (* rocksdb))
  (opt (* rocksdb-readoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* char))
  (keylen size-t)
  (vallen (* size-t)))

(define-alien-routine rocksdb-multi-get void
  (db (* rocksdb))
  (opt (* rocksdb-readoptions))
  (num-keys size-t)
  (keys-list (array c-string))
  (keys-list-sizes (array size-t))
  (values-list (array c-string))
  (values-list-sizes (array size-t))
  (errs (array rocksdb-errptr)))

(define-alien-routine rocksdb-multi-get-cf void
  (db (* rocksdb))
  (opt (* rocksdb-readoptions))
  (cfs (array rocksdb-column-family-handle))
  (num-keys size-t)
  (keys-list (array c-string))
  (keys-list-sizes (array size-t))
  (values-list (array c-string))
  (values-list-sizes (array size-t))
  (errs (array rocksdb-errptr)))

(export '(rocksdb-multi-get rocksdb-multi-get-cf))

(define-alien-routine rocksdb-cache-create-lru (* rocksdb) (capacity unsigned-int))

(export '(rocksdb-cache-create-lru))

(def-with-errptr rocksdb-flush void 
  (db (* rocksdb))
  (options (* rocksdb-flushoptions)))

(def-with-errptr rocksdb-flush-cf void
  (db (* rocksdb))
  (opts (* rocksdb-flushoptions))
  (cf (* rocksdb-column-family-handle))
  (num-cf int))

(def-with-errptr rocksdb-flush-cfs void
  (db (* rocksdb))
  (opts (* rocksdb-flushoptions))
  (cf (* (* rocksdb-column-family-handle)))
  (num-cf int))

(def-with-errptr rocksdb-flush-wal void
  (db (* rocksdb))
  (sync unsigned-char))

(define-alien-routine rocksdb-delete-file void
  (db (* rocksdb))
  (name c-string))

(define-alien-routine rocksdb-livefile (* rocksdb-livefiles)
  (db (* rocksdb))
  (name c-string))

(export '(rocksdb-delete-file rocksdb-livefile))

;; return NULL if prop name is unknown, else return pointer to
;; malloc-ed null-term value.
(define-alien-routine rocksdb-property-value (* t)
  (db (* rocksdb))
  (propname (* c-string)))

;; return 0 on success, else -1
(define-alien-routine rocksdb-property-int int
  (db (* rocksdb))
  (propname (* c-string)))

(define-alien-routine rocksdb-property-value-cf (* t)
  (db (* rocksdb))
  (cf (* rocksdb-column-family-handle))
  (propname (* c-string)))

(define-alien-routine rocksdb-property-int-cf int
  (db (* rocksdb))
  (cf (* rocksdb-column-family-handle))
  (propname (* c-string)))

(export '(rocksdb-property-value rocksdb-property-value-cf rocksdb-property-int rocksdb-property-int-cf))    

;;; CF
(def-with-errptr rocksdb-create-column-family 
  (* rocksdb-column-family-handle)
  (db (* rocksdb))
  (column-family-options (* rocksdb-options))
  (column-family-name c-string))

(def-with-errptr rocksdb-create-column-families 
  (array rocksdb-column-family-handle)
  (db (* rocksdb))
  (column-family-options (* rocksdb-options))
  (num-column-familes int)
  (column-family-names (array c-string))
  (lencfs (* size-t)))

(define-alien-routine rocksdb-create-column-families-destroy void
  (list (array rocksdb-column-family-handle)))

(define-alien-routine rocksdb-column-family-handle-destroy void
  (* rocksdb-column-family-handle))

(define-alien-routine rocksdb-column-family-handle-get-id unsigned-int
  (* rocksdb-column-family-handle))

(define-alien-routine rocksdb-column-family-handle-get-name c-string
  (handle (* rocksdb-column-family-handle))
  (name-len (* size-t)))

(export '(rocksdb-create-column-families-destroy rocksdb-create-column-family-handle-destroy
          rocksdb-column-family-handle-get-id rocksdb-column-family-handle-get-name))

(def-with-errptr rocksdb-drop-column-family 
  void
  (db (* rocksdb))
  (handle (* rocksdb-column-family-handle)))

(def-with-errptr rocksdb-open-column-families 
  (* rocksdb)
  (options (* rocksdb-options))
  (name c-string)
  (num-column-families int)
  (column-family-names (array c-string))
  (column-family-options (array rocksdb-options))
  (column-family-handles (array rocksdb-column-family-handle)))

(def-with-errptr rocksdb-list-column-families 
  (array c-string)
  (opt (* rocksdb-options))
  (name c-string)
  (lencf (* size-t)))

(define-alien-routine rocksdb-list-column-families-destroy void
  (list (array c-string))
  (len size-t))

(export '(rocksdb-list-column-families-destroy))

(def-with-errptr rocksdb-put-cf 
  void
  (db (* rocksdb))
  (opt (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* char))
  (keylen size-t)
  (val (* char))
  (vallen size-t))

(def-with-errptr rocksdb-delete-cf 
  void
  (db (* rocksdb))
  (options (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (key (* char))
  (keylen size-t))

(def-with-errptr rocksdb-delete-range-cf 
  void
  (db (* rocksdb))
  (options (* rocksdb-writeoptions))
  (cf (* rocksdb-column-family-handle))
  (start-key (* char))
  (start-key-len size-t)
  (end-key (* char))
  (end-key-len size-t))

(def-with-errptr rocksdb-disable-file-deletions void
  (db (* rocksdb)))

(def-with-errptr rocksdb-enable-file-deletions void
  (db (* rocksdb)))
  
(def-with-errptr rocksdb-destroy-db void
  (opts (* rocksdb-options))
  (name c-string))

(def-with-errptr rocksdb-repair-db void
  (opts (* rocksdb-options))
  (name c-string))

;;; Iterators
(define-alien-routine rocksdb-create-iterator (* rocksdb-iterator)
      (db (* rocksdb))
      (opt (* rocksdb-readoptions)))
(define-alien-routine rocksdb-iter-destroy void 
      (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-iter-seek-to-first void 
      (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-iter-valid boolean 
      (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-iter-next void 
      (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-iter-prev void 
      (iter (* rocksdb-iterator)))
(define-alien-routine rocksdb-iter-key (* char)
  (iter (* rocksdb-iterator))
  (klen-ptr (* size-t)))
(define-alien-routine rocksdb-iter-value (* char) 
  (iter (* rocksdb-iterator)) 
  (vlen-ptr (* size-t)))

(export '(rocksdb-create-iterator rocksdb-iter-destroy rocksdb-iter-seek-to-first rocksdb-iter-valid
          rocksdb-iter-next rocksdb-iter-prev rocksdb-iter-key rocksdb-iter-value))

;;; Backup
(def-with-errptr rocksdb-backup-engine-open
  (* rocksdb-backup-engine)
  (opts (* rocksdb-options))
  (path c-string))

(def-with-errptr rocksdb-backup-engine-create-new-backup
  void
  (be (* rocksdb-backup-engine))
  (db (* rocksdb)))

(def-with-errptr rocksdb-backup-engine-restore-db-from-latest-backup
  void
  (be (* rocksdb-backup-engine))
  (db-dir c-string)
  (wal-dir c-string)
  (res-opts (* rocksdb-restore-options)))

(def-with-errptr rocksdb-backup-engine-restore-db-from-backup
  void
  (be (* rocksdb-backup-engine))
  (db-dir c-string)
  (wal-dir c-string)
  (res-opts (* rocksdb-restore-options))
  (backup-id unsigned-int))

(define-alien-routine rocksdb-backup-engine-close void
  (be (* rocksdb-backup-engine)))

(export '(rocksdb-backup-engine-close))

;;; Transactions
(define-alien-routine rocksdb-transaction-begin (* rocksdb-transaction)
  (wopts (* rocksdb-writeoptions))
  (topts (* rocksdb-transaction-options))
  (told (* rocksdb-transaction)))

(def-with-errptr rocksdb-transactiondb-open
  (* rocksdb-transactiondb)
  (opts (* rocksdb-options))
  (topts (* rocksdb-transactiondb-options))
  (name c-string))

(define-alien-routine rocksdb-transactiondb-close void
  (tdb (* rocksdb-transactiondb)))

(export '(rocksdb-transaction-begin rocksdb-transaction-close))

;;; Perfcontext
(define-alien-routine rocksdb-set-perf-level void (val int))

(define-alien-routine rocksdb-perfcontext-create (* rocksdb-perfcontext))

(define-alien-routine rocksdb-perfcontext-reset void (* rocksdb-perfcontext))

(define-alien-routine rocksdb-perfcontext-report (* char) 
  (context (* rocksdb-perfcontext))
  (exclude-zero-counters unsigned-char))

(define-alien-routine rocksdb-perfcontext-metric unsigned-long
  (context (* rocksdb-perfcontext)) (metric int))

(define-alien-routine rocksdb-perfcontext-destroy void (* rocksdb-perfcontext))

(export '(rocksdb-perfcontext-reset rocksdb-perfcontext-report 
          rocksdb-perfcontext-metric rocksdb-perfcontext-destroy))

;;; Compaction Filter
;; (define-alien-routine rocksdb-compactionfilter-create (* rocksdb-compactionfilter)
;;   (state (* void))
;;   (destructor (* void))
;;   (filter (* unsigned-char))
;;   (name (* char)))

(define-alien-routine rocksdb-compactionfilter-set-ignore-snapshots void
  (self (* rocksdb-compactionfilter)) (val unsigned-char))

(define-alien-routine rocksdb-compactionfilter-destroy void
  (self (* rocksdb-compactionfilter)))

;;; Compaction Filter Context
(define-alien-routine rocksdb-compactionfiltercontext-is-full-compaction unsigned-char
  (context (* rocksdb-compactionfiltercontext)))

(define-alien-routine rocksdb-compactionfiltercontext-is-manual-compaction unsigned-char
  (context (* rocksdb-compactionfiltercontext)))

(export '(rocksdb-compactionfilter-set-ignore-snapshots rocksdb-compactionfilter-destroy
          rocksdb-compactionfiltercontext-is-full-compaction rocksdb-compactionfiltercontext-is-manual-compaction))

;;; Compaction Filter Factory

;;; Comparator
;; TODO 2023-12-11: 
;; (define-alien-routine rocksdb-comparator-create (* rocksdb-comparator)
;;   (state (* void))
;;   (destructor (* void))
;;   (compare (* int))
;;   (name (* char)))

(define-alien-routine rocksdb-comparator-destroy void (self (* rocksdb-comparator)))

;; (define-alien-routine rocksdb-comparator-with-ts-create (* rocksdb-comparator)
;;   (state (* void))
;;   (destructor (* void))
;;   (compare (* int))
;;   (compare-ts (* int))
;;   (compare-without-ts (* int))
;;   (name (* char)))

(export '(rocksdb-comparator-destroy))

;;; Filter Policy
(define-alien-routine rocksdb-filterpolicy-destroy void (self (* rocksdb-filterpolicy)))

(define-alien-routine rocksdb-filterpolicy-create-bloom (* rocksdb-filterpolicy)
  (bits-per-key double))

(define-alien-routine rocksdb-filterpolicy-create-bloom-full (* rocksdb-filterpolicy)
  (bits-per-key double))

(define-alien-routine rocksdb-filterpolicy-create-ribbon (* rocksdb-filterpolicy)
  (bloom-equivalent-bits-per-key double))

(define-alien-routine rocksdb-filterpolicy-create-ribbon-hybrid (* rocksdb-filterpolicy)
  (bloom-equivalent-bits-per-key double)
  (bloom-before-level int))

(export '(rocksdb-filterpolicy-destroy rocksdb-filterpolicy-create-bloom rocksdb-filterpolicy-create-bloom-full
          rocksdb-filterpolicy-create-ribbon rocksdb-filterpolicy-create-ribbon-hybrid))

;;; Merge Operator
;; TODO 2023-12-11: 
;; (define-alien-routine rocksdb-mergeoperator-create (* rocksdb-mergeoperator)
;;   (state (* void))
;;   (destructor (* void))
;;   (full-merge (* char))
;;   (partial-merge (* char))
;;   (delete-value (* void))
;;   (name (* char)))

(define-alien-routine rocksdb-mergeoperator-destroy void (self (* rocksdb-mergeoperator)))

(export '(rocksdb-mergeoperator-destroy))
