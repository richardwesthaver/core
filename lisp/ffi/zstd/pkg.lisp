;;; ffi/zstd/pkg.lisp --- ZSTD FFI

;; from zstd.h:
#|
  Introduction

  zstd, short for Zstandard, is a fast lossless compression algorithm, targeting
  real-time compression scenarios at zlib-level and better compression ratios.
  The zstd compression library provides in-memory compression and decompression
  functions.

  The library supports regular compression levels from 1 up to ZSTD_maxCLevel(),
  which is currently 22. Levels >= 20, labeled `--ultra`, should be used with
  caution, as they require more memory. The library also offers negative
  compression levels, which extend the range of speed vs. ratio preferences.
  The lower the level, the faster the speed (at the cost of compression).

  Compression can be done in:
    - a single step (described as Simple API)
    - a single step, reusing a context (described as Explicit context)
    - unbounded multiple steps (described as Streaming compression)

  The compression ratio achievable on small data can be highly improved using
  a dictionary. Dictionary compression can be performed in:
    - a single step (described as Simple dictionary API)
    - a single step, reusing a dictionary (described as Bulk-processing
      dictionary API)

  Advanced experimental functions can be accessed using
  `#define ZSTD_STATIC_LINKING_ONLY` before including zstd.h.

  Advanced experimental APIs should never be used with a dynamically-linked
  library. They are not "stable"; their definitions or signatures may change in
  the future. Only static linking is allowed.
|#

;;; Code:
(defpackage :zstd
  (:use :cl :std)
  (:nicknames :zstd))

(in-package :zstd)

(defun load-zstd (&key save)
  (prog1 (sb-alien:load-shared-object "libzstd.so" :dont-save (not save))
    (unless (member :zstd *features*)
      (push :zstd *features*))))
