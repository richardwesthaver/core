;;; lib/obj/seq.lisp --- Sequences

;; This package provides CLOS mixins for implementing sequences and iterators.

;; We use SBCL's extension to ANSI spec which allows subclassing of
;; the SEQUENCE class as well as the Iterator Protocol described in
;; the manual. Where possible, we avoid the Simple Iterator Protocol.

;; SB-SEQUENCE is similar to SB-POSIX in the sense that you're
;; supposed to use their package prefixes since they conflict with
;; symbols exported by CL. This package can be USEd in a DEFPACKAGE
;; form without conflicts.

;;; Code:
(in-package :obj/seq)

(defclass iterator ()
  ()
  (:documentation "Iterator superclass inherited by objects implementing the iterator protocol."))

(defclass ring ()
  ()
  (:documentation "Ring buffer protocol."))
