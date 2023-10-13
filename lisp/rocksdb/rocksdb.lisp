;;; rocksdb.lisp --- low-level bindings to the RocksDB C API

;; for the high-level interface, see rdb.lisp.

;;; Commentary:

;; if ur on archlinux and installed rocksdb via AUR you may receive an error from
;; jemalloc: cannot allocate memory in static TLS block:

;; https://github.com/veer66/cl-rocksdb/issues/1

;; for best results, you should compile rocksdb from source - use j0ni's snippet as a
;; starting point.

;; make shared_lib DISABLE_JEMALLOC=1 && 
;; sudo cp librocksdb.so.* /usr/local/lib/ && 
;; sudo cp -rf include/* /usr/local/include/

;; https://github.com/facebook/rocksdb/blob/main/Makefile

;; check /usr/local/include/rocksdb/c.h for the C API header, the source is under
;; db/c.cc

;;; Code:
(defpackage :rocksdb
  (:use :cl :sb-alien :macs.alien)
  (:export
   :load-rocksdb
   :rocksdb-open
   :rocksdb-close
   :rocksdb-destroy-db
   :rocksdb-put
   :rocksdb-get
   :rocksdb-delete
   :rocksdb-cancel-all-background-work
   :rocksdb-errptr
   ;; LRU CACHE
   :rocksdb-cache-create-lru
   ;; BLOCK-BASED OPTIONS
   :rocksdb-block-based-options-create
   :rocksdb-block-based-options-destroy
   :rocksdb-block-based-options-set-block-cache
   :set-block-based-options-cache-index-and-filter-blocks
   ;; OPTIONS
   :rocksdb-options-create
   :rocksdb-options-destroy
   :rocksdb-options-increase-parallelism
   :rocksdb-options-optimize-level-style-compaction
   :rocksdb-options-set-create-if-missing
   :rocksdb-options-set-block-based-table-factory
   :rocksdb-writeoptions-create
   :rocksdb-writeoptions-destroy
   :rocksdb-readoptions-create
   :rocksdb-readoptions-destroy
   ;; ITERATOR
   :rocksdb-iter-seek-to-first
   :rocksdb-iter-next
   :rocksdb-iter-prev
   :rocksdb-iter-valid
   :rocksdb-create-iterator
   :rocksdb-iter-key
   :rocksdb-iter-value
   :rocksdb-iter-destroy))

(in-package :rocksdb)

(defun load-rocksdb () 
  (unless (member :rocksdb *features*)
    (sb-alien:load-shared-object "librocksdb.so" :dont-save t)
    (push :rocksdb *features*)))

(load-rocksdb)  

;;; Opaque Types
(define-alien-type rocksdb (* t))
(define-alien-type rocksdb-options (* t))
(define-alien-type rocksdb-readoptions (* t))
(define-alien-type rocksdb-writeoptions (* t))
(define-alien-type rocksdb-compactoptions (* t))
(define-alien-type rocksdb-block-based-table-options (* t))
(define-alien-type rocksdb-iterator (* t))
(define-alien-type rocksdb-column-family-handle (* t))
(define-alien-type rocksdb-sstfilewriter (* t))
(define-alien-type rocksdb-errptr (* c-string))

;;; LRU
(define-alien-routine rocksdb-cache-create-lru (* rocksdb) (capacity unsigned-int))

;;; Options
;;;; block-based
(define-alien-routine rocksdb-block-based-options-create (* t))
(define-alien-routine rocksdb-block-based-options-destroy void 
  (options (* t)))
(define-alien-routine rocksdb-block-based-options-set-block-cache void 
  (options (* t)) 
  (block-cache (* t)))
(define-alien-routine rocksdb-block-based-options-set-cache-index-and-filter-blocks void
  (options (* t)) 
  (val c-string))
;;;; db
(define-alien-routine rocksdb-options-create rocksdb-options)
(define-alien-routine rocksdb-options-destroy void 
  (options rocksdb-options))
(define-alien-routine rocksdb-options-increase-parallelism void 
  (opt rocksdb-options) (total-threads int))
(define-alien-routine rocksdb-options-optimize-level-style-compaction void 
  (opt rocksdb-options) 
  (memtable_memory_budget (unsigned 4)))
(define-alien-routine rocksdb-options-set-create-if-missing void 
  (opt rocksdb-options) 
  (val boolean))
(define-alien-routine rocksdb-options-set-block-based-table-factory void
  (opt rocksdb-options)
  (table-options rocksdb-block-based-table-options))
;;;; write
(define-alien-routine rocksdb-writeoptions-create rocksdb-writeoptions)
(define-alien-routine rocksdb-writeoptions-destroy void
  (opt rocksdb-writeoptions))
;;;; read
(define-alien-routine rocksdb-readoptions-create rocksdb-readoptions)
(define-alien-routine rocksdb-readoptions-destroy void
  (opt rocksdb-readoptions))

;;; DB
(define-alien-routine rocksdb-open rocksdb 
  (opt rocksdb-options) 
  (name c-string) 
  (errptr rocksdb-errptr))
(define-alien-routine rocksdb-close void 
  (db rocksdb))
(define-alien-routine rocksdb-cancel-all-background-work void 
  (db rocksdb) 
  (wait boolean))

(define-alien-routine rocksdb-put void 
  (db rocksdb) 
  (options rocksdb-writeoptions) 
  (key c-string)
  (keylen size-t) 
  (val c-string)
  (vallen size-t) 
  (errptr rocksdb-errptr))

(define-alien-routine rocksdb-get c-string
  (db rocksdb) 
  (options rocksdb-readoptions) 
  (key c-string) 
  (keylen size-t) 
  (vallen (* size-t))
  (errptr rocksdb-errptr))

(define-alien-routine rocksdb-delete void
  (db rocksdb)
  (options rocksdb-writeoptions)
  (key c-string)
  (keylen size-t)
  (errptr rocksdb-errptr))

;;; Iterators
(define-alien-routine rocksdb-create-iterator rocksdb-iterator 
  (db rocksdb) 
  (opt rocksdb-readoptions))
(define-alien-routine rocksdb-iter-destroy void 
  (iter rocksdb-iterator))
(define-alien-routine rocksdb-iter-seek-to-first void 
  (iter rocksdb-iterator))
(define-alien-routine rocksdb-iter-valid boolean 
  (iter rocksdb-iterator))
(define-alien-routine rocksdb-iter-next void 
  (iter rocksdb-iterator))
(define-alien-routine rocksdb-iter-prev void 
  (iter rocksdb-iterator))
(define-alien-routine rocksdb-iter-key (* t) 
  (iter rocksdb-iterator) 
  (klen-ptr (* size-t)))
(define-alien-routine rocksdb-iter-value (* t) 
  (iter rocksdb-iterator) (vlen-ptr (* size-t)))
(define-alien-routine rocksdb-destroy-db void
  (options rocksdb-options)
  (name c-string) 
  (errptr rocksdb-errptr))
