#!/usr/bin/env -S sbcl --script
;; Bootstrap the core from a fresh SBCL installation.

;;; Commentary:

;; This script is intended for use with latest public release of SBCL.

;; from the project root:

;; sbcl --script bootstrap.lisp 

;;; Code:
(in-package :cl-user)
(require 'sb-md5)
(require 'sb-sprof)
(require 'sb-cover)
(require 'sb-grovel)
(require 'sb-posix)
(require 'sb-bsd-sockets)
(require 'sb-cltl2)
(require 'sb-concurrency)
(require 'sb-introspect)
(require 'sb-rotate-byte)
(require 'asdf)
(require 'uiop)
(asdf:load-asd (probe-file "ppcre/ppcre.asd"))
(asdf:load-asd (probe-file "std/std.asd"))
(asdf:load-system :ppcre)
(asdf:load-system :std)
(shadowing-import '(reset) :std)
(in-package :std-user)
(setq *stash* (make-pathname :directory (append (pathname-directory *default-pathname-defaults*) '(".stash"))))
(init :sys)
(let ((build-order 
        (list :std
              :ironclad/core
              :ironclad/ciphers
              :ironclad/digests
              :ironclad/macs
              :ironclad/prngs
              :ironclad/aeads
              :ironclad/kdfs
              :ironclad/public-keys
              :swank
              :swank/exts
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
              :q
              :rdb
              :organ
              :syn
              :nlp
              :doc
              :vc
              :box
              :pod
              :net
              :math
              :dsp
              :alien
              :skel
              :core)))
  (mapc 'load-system build-order))
(init :kbd :keysyms (stash-pathname "kbd.sxp") :input nil)
(gc :full t)
(make-system :core)
