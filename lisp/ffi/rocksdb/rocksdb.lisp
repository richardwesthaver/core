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

;; here are some important notes to keepin mind (from the API header):
#|
  C bindings for rocksdb.  May be useful as a stable ABI that can be
  used by programs that keep rocksdb in a shared library, or for
  a JNI api.

  Does not support:
  . getters for the option types
  . custom comparators that implement key shortening
  . capturing post-write-snapshot
  . custom iter, db, env, cache implementations using just the C bindings

  Some conventions:

  (1) We expose just opaque struct pointers and functions to clients.
  This allows us to change internal representations without having to
  recompile clients.

  (2) For simplicity, there is no equivalent to the Slice type.  Instead,
  the caller has to pass the pointer and length as separate
  arguments.

  (3) Errors are represented by a null-terminated c string.  NULL
  means no error.  All operations that can raise an error are passed
  a "char** errptr" as the last argument.  One of the following must
  be true on entry:
     *errptr == NULL
     *errptr points to a malloc()ed null-terminated error message
  On success, a leveldb routine leaves *errptr unchanged.
  On failure, leveldb frees the old value of *errptr and
  set *errptr to a malloc()ed error message.

  (4) Bools have the type unsigned char (0 == false; rest == true)

  (5) All of the pointer arguments must be non-NULL.|#

;;; Code:
(defpackage :rocksdb
  (:use :cl :alien)
  (:export
   :load-rocksdb
   ;; ERR
   :rocksdb-errptr
   ;; DB
   :rocksdb
   :rocksdb-open
   :rocksdb-close
   :rocksdb-destroy-db
   :rocksdb-put
   :rocksdb-get
   :rocksdb-delete
   :rocksdb-cancel-all-background-work
   ;; CACHE
   :rocksdb-cache
   :rocksdb-cache-create-lru
   ;; BLOCK-BASED OPTIONS
   :rocksdb-block-based-table-options
   :rocksdb-block-based-options-create
   :rocksdb-block-based-options-destroy
   :rocksdb-block-based-options-set-block-cache
   :set-block-based-options-cache-index-and-filter-blocks
   ;; OPTIONS
   :rocksdb-options
   :rocksdb-options-create
   :rocksdb-options-destroy
   :rocksdb-options-increase-parallelism
   :rocksdb-options-optimize-level-style-compaction
   :rocksdb-options-set-create-if-missing
   :rocksdb-options-set-block-based-table-factory
   ;; read
   :rocksdb-readoptions
   :rocksdb-readoptions-create
   :rocksdb-readoptions-destroy
   ;; write
   :rocksdb-writeoptions
   :rocksdb-writeoptions-create
   :rocksdb-writeoptions-destroy
   ;; compact
   :rocksdb-compactoptions
   ;; ITERATOR
   :rocksdb-iterator
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

;;; Alien Types
(define-alien-type rocksdb (struct rocksdb-t))
(define-alien-type rocksdb-options (struct rocksdb-options-t))
(define-alien-type rocksdb-readoptions (struct rocksdb-readoptions-t))
(define-alien-type rocksdb-writeoptions (struct rocksdb-writeoptions-t))
(define-alien-type rocksdb-compactoptions (struct rocksdb-compactoptions-t))
(define-alien-type rocksdb-block-based-table-options (struct rocksdb-block-based-table-options-t))
(define-alien-type rocksdb-iterator (struct rocksdb-iterator-t))
(define-alien-type rocksdb-cache (struct rocksdb-cache-t))
(define-alien-type rocksdb-column-family-handle (struct rocksdb-column-family-handler-t))
(define-alien-type rocksdb-sstfilewriter (struct rocksdb-sstfilewriter-t))

;; either (* void) or c-string (* (* char))
(define-alien-type rocksdb-errptr (* (* t)))

;;; Cache
(define-alien-routine rocksdb-cache-create-lru (* rocksdb) (capacity u32))

;;; Options

;;;; block-based
(define-alien-routine rocksdb-block-based-options-create (* rocksdb-block-based-table-options))
(define-alien-routine rocksdb-block-based-options-destroy void 
  (options (* rocksdb-block-based-table-options)))
(define-alien-routine rocksdb-block-based-options-set-block-cache void 
  (options (* rocksdb-block-based-table-options)) 
  (block-cache (* rocksdb-cache)))
(define-alien-routine rocksdb-block-based-options-set-cache-index-and-filter-blocks void
  (options (* rocksdb-block-based-table-options))
  (val c-string))

;;;; db
(define-alien-routine rocksdb-options-create (* rocksdb-options))
(define-alien-routine rocksdb-options-destroy void 
  (options rocksdb-options))
(define-alien-routine rocksdb-options-increase-parallelism void 
  (opt (* rocksdb-options)) (total-threads int))
(define-alien-routine rocksdb-options-optimize-level-style-compaction void 
  (opt (* rocksdb-options))
  (memtable-memory-budget u64))
(define-alien-routine rocksdb-options-set-create-if-missing void 
  (opt (* rocksdb-options))
  (val boolean))
(define-alien-routine rocksdb-options-set-block-based-table-factory void
  (opt (* rocksdb-options))
  (table-options (* rocksdb-block-based-table-options)))
;;;; write
(define-alien-routine rocksdb-writeoptions-create (* rocksdb-writeoptions))
(define-alien-routine rocksdb-writeoptions-destroy void
  (opt (* rocksdb-writeoptions)))
;;;; read
(define-alien-routine rocksdb-readoptions-create (* rocksdb-readoptions))
(define-alien-routine rocksdb-readoptions-destroy void
  (opt (* rocksdb-readoptions)))

;;; DB
(define-alien-routine rocksdb-open (* rocksdb)
  (opt (* rocksdb-options))
  (name c-string) 
  (errptr rocksdb-errptr))
(define-alien-routine rocksdb-close void 
  (db (* rocksdb)))
(define-alien-routine rocksdb-cancel-all-background-work void 
  (db (* rocksdb))
  (wait boolean))

(define-alien-routine rocksdb-put void 
  (db (* rocksdb))
  (options (* rocksdb-writeoptions))
  (key (* char))
  (keylen size-t) 
  (val (* char))
  (vallen size-t) 
  (errptr rocksdb-errptr))

(define-alien-routine rocksdb-get (* char)
  (db (* rocksdb))
  (options (* rocksdb-readoptions))
  (key (* char))
  (keylen size-t) 
  (vallen (* size-t))
  (errptr rocksdb-errptr))

(define-alien-routine rocksdb-delete void
  (db (* rocksdb))
  (options (* rocksdb-writeoptions))
  (key (* char))
  (keylen size-t)
  (errptr rocksdb-errptr))

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
  (iter (* rocksdb-iterator)) (vlen-ptr (* size-t)))
(define-alien-routine rocksdb-destroy-db void
  (options (* rocksdb-options))
  (name c-string) 
  (errptr rocksdb-errptr))
