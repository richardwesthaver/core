#!/usr/bin/env -S sbcl --script
#|Bootstrap the core from a fresh SBCL installation.

This script is intended for use with latest public release of SBCL.

from the project root:

$ sbcl --script bootstrap.lisp 
|#
(in-package :cl-user)
;; required sbcl features
(mapcar 
 'require 
 '(sb-md5 sb-sprof sb-cover sb-grovel 
   sb-posix sb-bsd-sockets sb-cltl2 sb-concurrency
   sb-introspect sb-rotate-byte asdf uiop))

;; load ppcre
(asdf:load-asd (probe-file "ppcre/ppcre.asd"))
(asdf:load-system :ppcre)

;; load std
(asdf:load-asd (probe-file "std/std.asd"))
(asdf:load-system :std)
#+nil (shadowing-import '(reset) :std)

(in-package :std-user)
(in-readtable :std)

;; initialize local *STASH*
(setq *stash* (make-pathname :directory (append (pathname-directory *default-pathname-defaults*) '(".stash"))))
(ensure-directories-exist *stash*)

;; set source location
(when #1=(sb-posix:getenv "SBCL_SRC") (sb-ext:set-sbcl-source-location (pathname #1#)))

;; set local SYS:SITE for inter-op with LOAD-LOGICAL-PATHNAME-TRANSLATIONS
(setf (logical-pathname-translation "SYS" "SITE;*.*.*") (merge-pathnames "etc/lisp/*.*"))
;; overwrite all logical paths
(mapcar 'load-logical-host '("SYS" "ETC" "USR" "VAR" "SRV" "USER" "SKEL" "MPK" "PACKY"))

(init :sys)
(let ((build-order 
        (list :std
              :ironclad
              :ironclad/cipher
              :ironclad/digest
              :ironclad/mac
              :ironclad/prng
              :ironclad/aead
              :ironclad/kdf
              :ironclad/public-key
              :swank
              :swank/ext
              :xkb
              :evdev
              :btrfs
              :zstd
              :sys
              :keyutils
              :tree-sitter
              :obj
              :uring
              :io
              :log
              :arrow
              :glib
              :gstreamer
              :rocksdb
              :blas
              :lapack
              :cuda
              :openssl
              :ssh2
              :jpeg
              :dat
              :rt
              :sndfile
              :alsa
              :chromaprint
              :jack
              :ffmpeg
              :parse
              :cry
              :cli
              :net
              :q
              :rdb
              :organ
              :syn
              :doc
              :vc
              :box
              :pod
              :math
              :dsp
              :alien
              :skel
              :core)))
  (mapc 'load-system build-order))

;; remap MEDIA logical host
(dsp:load-media-logical-host #l"mpk:media;")

;; init keyboard support
(init :kbd :keysyms (stash-pathname "kbd.sxp") :input nil)

;; perform full gc
(dotimes (i 10)
  (gc :full t))

;; save the core and exit
(make-system :core)
