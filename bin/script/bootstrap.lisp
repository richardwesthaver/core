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
  (asdf:load-system :std))
(shadowing-import '(reset) :std)
(in-package :std-user)
(std:init :sys)
(std:load-sys (probe-file "~/comp/shed/ppcre/ppcre.sys"))
(std:load-sys (probe-file "~/comp/shed/ironclad/ironclad.sys"))
;; (load-system :std)
