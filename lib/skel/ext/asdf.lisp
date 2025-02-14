;;; asdf.lisp --- ASDF system support for Skel

;; This package contains middleware between ASDF and Skel.

;;; Commentary:

;; ASDF is the defacto system management facility for Common Lisp
;; implementations and we intend to fully interop with it. 

;; Our design consists of two modules:

;; - An ASDF plugin for integrating skel into ASDF system definitions

;; - An ASDF system definition compiler - defined in the package :skel/comp/asd

;; Refs: 

;; - https://github.com/atlas-engineer/nyxt/tree/master/libraries/nasdf

;;; Code:
(in-package :skel/ext/asdf)
