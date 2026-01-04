;;; bootstrap.lisp --- Core Bootstrap Script

;; Bootstrap the core from a fresh SBCL installation.

;;; Commentary:

;; This script is intended for use with latest public release of SBCL.

;; from the project root:

;; sbcl --script bin/script/bootstrap.lisp 

;;; Code:
(in-package :cl-user)
(require 'asdf)
(require 'sb-cltl2)
(require 'sb-concurrency)
(require 'sb-sprof)
(require 'sb-introspect)
(require 'sb-rotate-byte)
;; (asdf:load-system :ppcre)
(flet ((%load (f) (asdf:load-asd (probe-file f))))
  (%load "~/comp/shed/ppcre/ppcre.asd")
  (%load "~/comp/shed/ironclad/ironclad.asd")
  (%load "std/std.asd"))

(asdf:load-system :std)
(in-package :std-user)
(init :sys :sysdefs (sysdefs *default-pathname-defaults*))

(setq *asdf-compatibility* t)
