;;; io/pkg.lisp --- high-level IO API

;;

;;; Commentary:

;; pay close attention to the spec for opportunities to replace io
;; primitives -- for example WITH-OPEN-FILE accepts a :CLASS keyword
;; argument, which defaults to SB-SYS:FD-STREAM.

;; this package would be responsible for providing an alternative
;; class, something like IO-STREAM.

;;; Code:
(defpackage :io/proto
  (:use :cl :std/condition)
  (:export :io-error))

(defpackage :io/static-vector
  (:use :cl :std :sb-alien)
  (:shadow :constantp)
  (:export
   ;; Constructors and destructors
   :make-static-vector
   :free-static-vector
   :with-static-vector
   :with-static-vectors
   ;; Accessors
   :static-vector-pointer
   ;; Alien wrapper type
   :static-vector
   ;; Foreign memory operations
   :replace-foreign-memory
   :fill-foreign-memory))

(defpackage :io/fast
  (:use :cl :std :io/proto)
  (:import-from :io/static-vector :make-static-vector)
  (:export
   #:fast-read-byte #:fast-write-byte
   #:fast-read-sequence #:fast-write-sequence
   #:with-fast-input #:with-fast-output
   #:write8 #:writeu8
   #:write8-le #:writeu8-le #:write8-be #:writeu8-be
   #:write16-le #:writeu16-le #:write16-be #:writeu16-be
   #:write24-le #:writeu24-le #:write24-be #:writeu24-be
   #:write32-le #:writeu32-le #:write32-be #:writeu32-be
   #:write64-le #:writeu64-le #:write64-be #:writeu64-be
   #:write128-le #:writeu128-le #:write128-be #:writeu128-be
   #:read8 #:readu8
   #:read8-le #:readu8-le #:read8-be #:readu8-be
   #:read16-le #:readu16-le #:read16-be #:readu16-be
   #:read32-le #:readu32-le #:read32-be #:readu32-be
   #:read64-le #:readu64-le #:read64-be #:readu64-be
   #:read128-le #:readu128-le #:read128-be #:readu128-be
   #:fast-output-stream #:fast-input-stream))

(defpackage :io/ring
  (:use :cl :uring :io/proto)
  (:import-from :sb-alien :addr)
  (:import-from :std :deferror :eval-always))

(defpackage :io/stream
  (:use :cl :io/proto)
  (:import-from :std :deferror :eval-always)
  (:export :io-stream-error :io-stream))

(defpackage :io/socket
  (:use :cl :io/proto)
  (:import-from :std :deferror :eval-always)
  (:export :io-socket-error :io-socket :sockopt-receive-timeout))

(defpackage :io/flate
  (:use :cl :io/proto)
  (:import-from :std :deferror :eval-always)
  (:export :flate-error :compression-error :decompression-error
   :*compression-buffer-size* :decompression-buffer-size* :finish-compression :finish-decompression
   :reset-compressor :reset-decompressor :make-compressing-stream :make-decompressing-stream
   :compress-object :decompress-object :compress :decompress
   :compressor :compressing-stream :decompressor :decompressing-stream))

(defpackage :io/zstd
  (:use :cl :std :io/proto :io/flate)
  (:import-from :std :deferror :eval-always)
  (:export :zstd-error :zstd-compressor :zstd-decompressor))

(defpackage :io/kbd
  (:use :cl :std :io/proto :xkb)
  (:export :kbd-error))

(pkg:defpkg :io
  (:use :cl)
  (:use-reexport :io/proto :io/ring :io/flate :io/zstd :io/stream :io/socket))
