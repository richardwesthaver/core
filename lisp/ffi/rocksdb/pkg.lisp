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
  (:use :cl :std :sb-alien)
  (:export
   :load-rocksdb
   :*rocksdb-options*
   :*rocksdb-compaction-levels*
   :*rocksdb-compression-backends*
   :*rocksdb-perf-metrics*
   :*rocksdb-statistics-levels*))

(in-package :rocksdb)

(defun load-rocksdb () 
  (unless (member :rocksdb *features*)
    (sb-alien:load-shared-object "librocksdb.so" :dont-save t)
    (push :rocksdb *features*)))

;; (load-rocksdb)
