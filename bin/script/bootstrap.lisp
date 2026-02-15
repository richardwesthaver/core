;;; bootstrap.lisp --- Core Bootstrap Script

;; Bootstrap the core from a fresh SBCL installation.

;;; Commentary:

;; This script is intended for use with latest public release of SBCL.

;; from the project root:

;; sbcl --script bin/script/bootstrap.lisp 

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

(progn
  (asdf:load-asd (probe-file "~/comp/shed/ppcre/ppcre.asd"))
  (asdf:load-asd (probe-file "~/comp/core/std/std.asd"))
  (asdf:load-system :ppcre)
  (asdf:load-system :std :force t))
(shadowing-import '(reset) :std)
(in-package :std-user)
(setq *stash* (make-pathname :directory (append (pathname-directory *default-pathname-defaults*) '(".stash"))))
(setq *user-fasl-cache* (merge-pathnames (make-pathname :directory '(:relative "cache" "lisp")) *stash*))
(init :sys :fasl-cache *user-fasl-cache*)
(load-system :ironclad)
;; ffi
(compile-system :uring)
(compile-system :xkb)
(compile-system :evdev)
(compile-system :btrfs)
(compile-system :zstd)
(compile-system :syslog)
(compile-system :blake3)
(compile-system :keyutils)
(compile-system :tree-sitter)
(load-system :uring)
(load-system :zstd)
(load-system :btrfs)
(load-system :log)
(compile-system :rocksdb)
(compile-system :cuda)
(compile-system :openssl)
(compile-system :ssh2)
(load-system :openssl)
(load-system :io)
(load-system :dat)
(load-system :rt)
(compile-system :jpeg)
(compile-system :sndfile)
(compile-system :alien)
(load-system :parse)
(load-system :cry)
(load-system :rdb)
(load-system :organ)
(load-system :syn)
(load-system :cli)
(load-system :net)
(load-system :math)
(load-system :dsp)
(compile-system :skel)
(load-system :skel)
(compile-system :core)
(load-system :core)
(load-system :bin)
(funcall (find-module :bin :bin))
