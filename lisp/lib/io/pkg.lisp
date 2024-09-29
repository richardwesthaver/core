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
