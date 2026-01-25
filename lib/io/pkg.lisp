;;; io/pkg.lisp --- high-level IO API

;;

;;; Commentary:

;; pay close attention to the spec for opportunities to replace io
;; primitives -- for example WITH-OPEN-FILE accepts a :CLASS keyword
;; argument, which defaults to SB-SYS:FD-STREAM.

;; this package would be responsible for providing an alternative
;; class, something like IO-STREAM.

;;; Code:
(pkg:defpkg :io-int
  (:use :cl :std)
  (:export :*io-packages*))

(in-package :io-int)
(defparameter *io-packages* nil)
(setq *defpkg-hook* (lambda (x) (pushnew (package-name x) *io-packages* :test 'string=)))

(defpkg :io/proto
  (:use :cl :std/condition)
  (:export :io-error
   :output :input
   :output-size :input-size
   :output-buffer :input-buffer
   :input-position :output-position
   :output-available-p :input-available-p
   :fill-buffer :header
   :header-type :header-length
   :offset
   :snapshot))

(defpkg :io/stream
  (:use :cl :io/proto :sb-gray :std/meta)
  (:import-from :std :deferror :eval-always :stream-of :wrapped-stream)
  (:export :io-stream-error :io-stream :make-bound-stream
   :bound-input-stream :ensure-file-position
   :peeking-input-stream :peeked
   :peeked-count
   :peeked-size
   :make-flex-stream
   :make-in-memory-output-stream
   :make-in-memory-input-stream
   :with-output-to-sequence
   :with-input-from-sequence
   :output-stream-sequence-length
   :get-output-stream-sequence
   :peek-byte
   :peeked
   :flex-stream
   :flex-input-stream
   :flex-output-stream
   :flex-io-stream
   :make-decoding-stream
   :decoding-stream
   :+buffer-size+
   :dec-fill-buffer
   :needs-to-fill-buffer-p))

(defpkg :io/static
  (:use :cl :std :sb-alien :io/stream)
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
   :fill-foreign-memory
   :static-stream
   :*default-static-stream-size*
   :with-static-stream
   :with-static-streams
   :reset-static-stream))

(defpkg :io/fast
  (:use :cl :std :io/proto :io/stream)
  (:import-from :io/static :make-static-vector)
  (:import-from :std/macs :once-only)
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

(defpkg :io/uring
  (:use :cl :uring :io/proto)
  (:import-from :sb-alien :addr)
  (:import-from :std :deferror :eval-always))

(defpkg :io/chunky
  (:nicknames :chunky)
  (:use :cl :std/stream :io/proto :io/stream :sb-gray :std/meta)
  (:import-from :std :deferror :when-let :define-constant :eval-always)
  (:export
   #:output-chunking-p
   #:chunked-input-stream
   #:chunked-stream
   :chunked-output-stream
   #:+default-chunked-output-size+
   #:input-chunking-p
   #:simple-chunked-input-stream
   #:chunked-input-stream-extensions
   #:chunked-input-stream-trailers
   #:signal-eof
   #:expecting-crlf-p
   #:chunked-io-stream
   #:make-chunked-stream
   #:blocked-stream
   #:blocked-io-stream
   #:blocked-output-stream
   #:blocked-input-stream
   #:read-char*
   #:unread-char*
   #:peek-char*
   #:assert-char
   #:assert-crlf
   #:with-character-stream-semantics
   #:*accept-bogus-eols*
   #:*treat-semicolon-as-continuation*))

(defpkg :io/socket
  (:use :cl :io/proto :sb-alien)
  (:import-from :std :deferror :eval-always :timeval)
  (:export :io-socket-error 
   :io-socket :sockopt-receive-timeout :sockopt-send-timeout :sockopt-linger
   :sockopt-peercred))

(defpkg :io/flate
  (:use :cl :io/proto :sb-gray)
  (:import-from :std :deferror :eval-always)
  (:import-from :std/stream :wrapped-stream)
  (:export :flate-error :compression-error :decompression-error
   :*compression-buffer-size* :decompression-buffer-size* :finish-compression :finish-decompression
   :reset-compressor :reset-decompressor
   :compress-object :decompress-object :compress :decompress
   :compressor :compressing-stream :decompressor :decompressing-stream
   :make-decompressing-stream :make-compressing-stream
   :*decompression-buffer-size* :*compression-level*
   :compress-with :decompress-with
   :compression-level :*compressor*
   :*decompressor* :*preferred-compression-type*
   :*compression-types*
   :compress-octet-vector
   :flush
   :compress-octet
   :with-compressor))

(defpkg :io/zstd
  (:use :cl :std :io/proto :io/flate :sb-alien)
  (:import-from :zstd :zstd-createdstream :zstd-createcstream
   :zstd-dstream :zstd-cstream :zstd-freecstream :zstd-freedstream
   :with-zstd-dstream :with-zstd-cstream :zstd-initcstream :zstd-initdstream
   :zstd-compressstream2 :zstd-decompressstream
   :allocate-zstd-inbuffer :allocate-zstd-outbuffer :zstd-outbuffer :zstd-inbuffer
   :zstd-inbuffer-src :zstd-inbuffer-size :zstd-outbuffer-dst :zstd-outbuffer-size
   :zstd-enddirective :zstd-dstreaminsize :zstd-dstreamoutsize :zstd-cstreaminsize 
   :zstd-cstreamoutsize :zstd-inbuffer-pos :zstd-outbuffer-pos)
  (:import-from :std :deferror :eval-always)
  (:import-from :sb-gray :stream-force-output :stream-finish-output
   :stream-write-sequence)
  (:export :zstd-error :zstd-compressor :zstd-decompressor
   :with-zstd-output :with-zstd-input
   :with-zstd-buffer :with-zstd-stream))

(defpkg :io/deflate
  (:use :cl :std :io/proto :io/flate)
  (:import-from :std :deferror :eval-always)
  (:import-from :sb-gray :stream-force-output :stream-finish-output
   :stream-write-sequence)
  (:import-from :ironclad :make-digest :produce-digest :update-digest :copy-digest)
  (:export :inflate-state :bzip2-state :make-dstate 
   :finish-dstate :make-inflate-state :finish-inflate-state :deflate
   :zlib :gzip :bzip2 :invalid-format-error
   #:invalid-checksum-error
   #:premature-end-of-stream
   #:inflate-error
   #:invalid-zlib-header-error
   #:invalid-gzip-header-error
   #:reserved-block-type-error
   #:invalid-stored-block-length-error
   #:bzip2-error
   #:invalid-bzip2-data
   :deflate-compressor
   :zlib-compressor
   :gzip-compressor))

(defpkg :io/kbd
  (:use :cl :std :io/proto :xkb :evdev :sb-alien)
  (:export :kbd-error
   :load-kbd-libs
   :keyboard
   :keysym
   :device-read-event
   :print-device-input-info
   :make-keyboard-from-dev
   :keyboard-device-p
   :new-device-from-path
   :evdev-bit-p
   :kbd-code-name
   :get-keyboards))

(defpkg io/xsubseq
  (:use :cl)
  (:import-from :sb-cltl2 :variable-information)
  (:import-from :std/type :octet-vector)
  (:export :xsubseq
   :octet-xsubseq :string-xsubseq
   :concatenated-xsubseqs :null-concatenated-xsubseqs
   :octet-concatenated-xsubseqs :string-concatenated-xsubseqs
   :make-concatenated-xsubseqs :xlength
   :xnconc :xnconcf
   :coerce-to-sequence :coerce-to-string
   :with-xsubseqs))

(defpkg :io/smart-buffer
  (:use :cl :io/xsubseq)
  (:export :*default-memory-limit*
   :*default-disk-limit* :smart-buffer
   :make-smart-buffer :write-to-buffer
   :finalize-buffer :with-smart-buffer
   :buffer-on-memory-p :delete-stream-file
   :delete-temporary-files :buffer-limit-exceeded))
