;;; ffi/zstd/pkg.lisp --- ZSTD FFI

;; Zstd compression support for Lisp

;;; Commentary:

;; Initially I was thinking of this as an SB-CONTRIB module which links up
;; with whatever C runtime functions exposed by the built-in SBCL compression
;; support. However, there isn't actually much going on in the runtime and
;; it's not publicly exposed at all. The SBCL/Zstd surface-area is restrained
;; to FASL read/write streams and not of much use outside it.

;; So, we'll be applying the same from-scratch strategy we've become
;; accustomed to, exposing as much of the C API as possible and building up
;; our abstractions.

;; The low-level abstractions are in this package - ZSTD.

;; For the high-level abstractions see IO/FLATE and IO/ZSTD in the IO package.

;; The following programs have compile-time support for linking Zstd:

;; SBCL
;; QEMU
;; RocksDB 

#| from zstd.h:
Introduction                            ; ; ; ; ; ;
                                        ; ; ; ; ; ;
zstd, short for Zstandard, is a fast lossless compression algorithm, targeting ; ; ; ; ; ;
real-time compression scenarios at zlib-level and better compression ratios. ; ; ; ; ; ;
The zstd compression library provides in-memory compression and decompression ; ; ; ; ; ;
functions.                              ; ; ; ; ; ;
                                        ; ; ; ; ; ;
The library supports regular compression levels from 1 up to ZSTD_maxCLevel(), ; ; ; ; ; ;
which is currently 22. Levels >= 20, labeled `--ultra`, should be used with ; ; ; ; ; ;
caution, as they require more memory. The library also offers negative ; ; ; ; ; ;
compression levels, which extend the range of speed vs. ratio preferences. ; ; ; ; ; ;
The lower the level, the faster the speed (at the cost of compression). ; ; ; ; ; ;
                                        ; ; ; ; ; ;
Compression can be done in:             ; ; ; ; ; ;
- a single step (described as Simple API) ; ; ; ; ; ;
- a single step, reusing a context (described as Explicit context) ; ; ; ; ; ;
- unbounded multiple steps (described as Streaming compression) ; ; ; ; ; ;
                                        ; ; ; ; ; ;
The compression ratio achievable on small data can be highly improved using ; ; ; ; ; ;
a dictionary. Dictionary compression can be performed in: ; ; ; ; ; ;
- a single step (described as Simple dictionary API) ; ; ; ; ; ;
- a single step, reusing a dictionary (described as Bulk-processing ; ; ; ; ; ;
dictionary API)                         ; ; ; ; ; ;
                                        ; ; ; ; ; ;
Advanced experimental functions can be accessed using ; ; ; ; ; ;
`#define ZSTD_STATIC_LINKING_ONLY` before including zstd.h. ; ; ; ; ; ;
                                        ; ; ; ; ; ;
Advanced experimental APIs should never be used with a dynamically-linked ; ; ; ; ; ;
library. They are not "stable"; their definitions or signatures may change in ; ; ; ; ; ;
the future. Only static linking is allowed. ; ; ; ; ; ;
|#

;;; Code:
(defpackage :zstd
  (:use :cl :std :sb-alien)
  (:nicknames :zstd)
  (:export :zstd-alien-error :with-zstd-cstream :with-zstd-dstream
   :zstd-versionnumber :zstd-cstreaminsize :zstd-cstreamoutsize :zstd-inbuffer
   :zstd-iserror :zstd-defaultclevel :zstd-compress :zstd-decompress
   :zstd-cstream :zstd-dstream :zstd-compressstream :zstd-decompressstream
   :zstd-compressstream2 :zstd-outbuffer :zstd-geterrorname :zstd-geterrorcode
   :zstdc :zstdd
   :zstd-alien-error :zstd-dstream-error :zstd-cstream-error))

(in-package :zstd)

(define-alien-loader "zstd" t "/usr/lib/")

;;; Types
(deftype zstd-error-code ()
  `(integer 0 120))

(deftype zstd-strategy-designator ()
  `(or (integer ,(zstd-minclevel) ,(zstd-maxclevel))
       (member :fast :dfast :greedy :lazy
               :lazy2 :btlazy2 :btopt :btultra
               :btultra2)))

(deftype zstd-compression-parameter ()
  `(integer 100 1024))
(deftype zstd-decompression-parameter ()
  `(integer 100 1024))

(deftype zstd-reset-directive ()
  `(or (integer 1 3) (member :session-only :parameters :session-and-parameters)))
(deftype zstd-end-directive ()
  `(or (integer 0 2) (member :continue :flus :end)))

;;; Errors
(define-condition zstd-alien-condition () ()
  (:documentation "Superclass of all conditions triggered by the ZSTD FFI."))

(deferror zstd-alien-error (error)
    ((code :initarg :code :accessor zstd-error-code))
    (:documentation "Error signaled from Zstd Alien."))
    
;; found in zstd_errors.h
(define-alien-enum (zstd-errorcode int)
                   :no-error 0
                   :generic 1
                   :prefix-unknown 10
                   :version-unsupported 12
                   :frameparameter-unsupported 14
                   :frameparameter-windowtoolarge 16
                   :corruption-detected 20
                   :checksum-wrong 22
                   :literals-headerwrong 24
                   :dictionary-corrupted 30
                   :dictionary-wrong 32
                   :dictionarycreation-failed 34
                   :parameter-unsupported 40
                   :parameter-combination-unsupported 41
                   :parameter-outofbound 42
                   :tablelog-toolarge 44
                   :maxsymbolvalue-toolarge 46
                   :maxsymbolvalue-toosmall 48
                   :stabilitycondition-notrespected 50
                   :stage-wrong 60
                   :init-missing 62
                   :memory-allocation 64
                   :workspace-toosmall 66
                   :dstsize-toosmall 70
                   :srcsize-wrong 72
                   :dstbuffer-null 74
                   :noforwardprogress-destfull 80
                   :noforwardprogress-inputempty 82
                   ;; unstable
                   :frameindex-toolarge 100
                   :seekableio 102
                   :dstbuffer-wrong 104
                   :srcbuffer-wrong 105
                   :sequenceproducer-failed 106
                   :externalsequences-invalid 107
                   :maxcode 120)

;;; Utils
(define-alien-routine "ZSTD_versionNumber" unsigned)
(define-alien-routine "ZSTD_versionString" c-string)
(define-alien-routine "ZSTD_compressBound" size-t (src-size size-t))
(define-alien-routine "ZSTD_isError" unsigned (code size-t))
(define-alien-routine "ZSTD_getErrorName" c-string (code size-t))
;; zstd_errors.h - does this work?
(define-alien-routine "ZSTD_getErrorCode" int (function-result size-t))
(define-alien-routine "ZSTD_getErrorString" c-string (code int))

(define-alien-routine "ZSTD_minCLevel" int)
(define-alien-routine "ZSTD_maxCLevel" int)
(define-alien-routine "ZSTD_defaultCLevel" int)

(define-alien-routine "ZSTD_findFrameCompressedSize" size-t
  (src (* t))
  (src-size size-t))

(define-alien-routine "ZSTD_getFrameContentSize" unsigned-long-long
  (src (* t))
  (src-size size-t))

(define-alien-routine "ZSTD_decompressBound" unsigned-long-long
  (src (* t))
  (src-size size-t))

;;; Explicit Context API
(define-alien-type zstd-cctx (struct zstd-cctx-s))

(define-alien-routine "ZSTD_createCCtx" (* zstd-cctx))
(define-alien-routine "ZSTD_freeCCtx" void (cctx (* zstd-cctx)))
(define-alien-routine "ZSTD_compressCCtx" size-t
  (cctx (* zstd-cctx))
  (dst (* t)) (dst-capacity size-t)
  (src (* t)) (src-size size-t)
  (compression-level int))

(define-alien-type zstd-dctx (struct zstd-dctx-s))

(define-alien-routine "ZSTD_createDCtx" (* zstd-dctx))
(define-alien-routine "ZSTD_freeDCtx" void (dctx (* zstd-dctx)))
(define-alien-routine "ZSTD_decompressDCtx" size-t
  (dctx (* zstd-dctx))
  (dst (* t)) (dst-capacity size-t)
  (src (* t)) (src-size size-t))
;;; Advanced API
(define-alien-enum (zstd-strategy int)
                   :fast 1
                   :dfast 2
                   :greedy 3
                   :lazy 4
                   :lazy2 5
                   :btlazy2 6
                   :btopt 7
                   :btultra 8
                   :btultra2 9)

(define-alien-enum (zstd-cparameter int)
                   :compression-level 100
                   :window-log 101
                   :hash-log 102
                   :chain-log 103
                   :search-log 104
                   :min-match 105
                   :target-length 106
                   :strategy 107
                   :target-c-block-size 130
                   :enable-long-distance-matching 160
                   :ldm-hash-log 161
                   :ldm-min-match 162
                   :ldm-bucket-size-log 163
                   :ldm-hash-rate-log 164
                   :content-size-flag 200
                   :checksum-flag 201
                   :dict-id-flag 202
                   :nb-workers 400
                   :job-size 401
                   :overlap-log 402
                   :expiremental1 500
                   :expiremental2 10
                   :expiremental3 1000
                   :expiremental4 1001
                   :expiremental5 1002
                   ;; :expiremental6 1003 ;; is now target-c-block-size
                   :expiremental7 1004
                   :expiremental8 1005
                   :expiremental9 1006
                   :expiremental10 1007
                   :expiremental11 1008
                   :expiremental12 1009
                   :expiremental13 1010
                   :expiremental14 1011
                   :expiremental15 1012
                   :expiremental16 1013
                   :expiremental17 1014
                   :expiremental18 1015
                   :expiremental19 1016)

(define-alien-enum (zstd-reset-directive int)
                   :session-only 1
                   :parameters 2
                   :session-and-parameters 3)

(define-alien-enum (zstd-dparameter int)
                   :window-log-max 100
                   :experimental1 1000
                   :experimental2 1001
                   :experimental3 1002
                   :experimental4 1003                   
                   :experimental5 1004
                   :experimental6 1005)
