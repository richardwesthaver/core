#!/usr/bin/env -S sbcl --script
#|Bootstrap the core from a fresh SBCL installation.

This script is intended for use with latest public release of SBCL.

from the project root:

$ sbcl --script bootstrap.lisp 
|#
(in-package :cl-user)
;; required sbcl features
(dolist (m '(sb-md5 sb-sprof sb-cover sb-grovel 
             sb-posix sb-bsd-sockets sb-cltl2 sb-concurrency
             sb-introspect sb-rotate-byte sb-simd uiop))
  (require m))

;; load std
(let ((*default-pathname-defaults* (merge-pathnames "std/"))
      (std-sys))
  (with-open-file (sys "std.sys")
    ;; skip DEFSYS :STD "docstring", remainder of form is a plist.
    (setf std-sys (cdddr (read sys))))
  ;; load components in sequence
  (labels ((%load (component)
             (destructuring-bind (type name &rest args) component
               (case type
                 (:mod ; load all sub-components
                  (let ((*default-pathname-defaults* (probe-file (string-downcase name))))
                    (mapc #'%load (cadr args))))
                 (:dir ; load directory contents
                  (mapc #'load (directory (concatenate 'string (string-downcase name) "/*.lisp"))))
                 (t (load (make-pathname :name (string-downcase name) :type "lisp" :directory '(:relative))))))))
    (dolist (c (getf std-sys :components)) (%load c))))

(in-package :std-user)
(in-readtable :std)

;; initialize local *STASH*
(setq *stash* (make-pathname :directory (append (pathname-directory *default-pathname-defaults*) '(".stash"))))
(ensure-directories-exist *stash*)

;; set source location
(when #1=(sb-posix:getenv "SBCL_SRC") (sb-ext:set-sbcl-source-location (pathname #1#)))

(init :sys)
;; set local SYS:SITE for inter-op with LOAD-LOGICAL-PATHNAME-TRANSLATIONS
(setf (logical-pathname-translation "SYS" "SITE;*.*.*") (merge-pathnames "etc/lisp/*.*"))
;; overwrite all logical paths
(mapcar 'load-logical-host '("SYS" "ETC" "USR" "VAR" "SRV" "USER" "SKEL" "MPK" "PACKY"))

(defvar *core-build-order*
  (list :std
        :ironclad
        :swank
        :swank/ext
        :xkb
        :evdev
        :btrfs
        :zstd
        :sys
        :keyutils
        :tree-sitter
        :uring
        :io
        :obj
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
        :core))

(mapc 'load-system *core-build-order*)

;; remap MEDIA logical host
(dsp:load-media-logical-host #l"mpk:media;")

;; init keyboard support
(init :kbd :keysyms (stash-pathname "kbd.sxp") :input nil)

;; perform full gc
(dotimes (i 10)
  (gc :full t))

;; save the core and exit
(make-system :core)
