;;; bqn.lisp --- low-level bindings to CBQN

;;; Commentary:

;;; Code:
(defpackage :bqn/pkg
  (:nicknames :bqn)
  (:use :cl :std)
  (:export 
   :load-bqn
   :bqnv
   :bqn-init
   :bqn-free
   :bqn-copy
   :bqn-tof64
   :bqn-tochar
   :bqn-readf64
   :bqn-readchar
   :bqn-type
   :bqn-call1
   :bqn-call2
   :bqn-eval
   :bqn-evalcstr
   :bqn-bound
   :bqn-rank
   :bqn-shape
   :bqn-pick
   :bqn-readi8arr
   :bqn-readi16arr
   :bqn-readi32arr
   :bqn-readf64arr))

(in-package :bqn)

(defun load-bqn () 
  (unless (member :bqn *features*)
    (sb-alien:load-shared-object "libcbqn.so" :dont-save t)
    (push :bqn *features*)))

(define-alien-type bqnv unsigned-long)
(define-alien-routine bqn-init void)
(define-alien-routine bqn-free void (v bqnv))
(define-alien-routine bqn-copy bqnv (v bqnv))
(define-alien-routine bqn-tof64 double-float (v bqnv))
(define-alien-routine bqn-tochar unsigned-int (v bqnv))
(define-alien-routine bqn-readf64 double-float (v bqnv))
(define-alien-routine bqn-readchar unsigned-int (v bqnv))
(define-alien-routine bqn-type int (v bqnv))
(define-alien-routine bqn-call1 bqnv (f bqnv) (x bqnv))
(define-alien-routine bqn-call2 bqnv (f bqnv) (w bqnv) (x bqnv))
(define-alien-routine bqn-eval bqnv (src bqnv))
(define-alien-routine bqn-evalcstr bqnv (str c-string))
(define-alien-routine bqn-bound size-t (a unsigned-long))
(define-alien-routine bqn-rank size-t (a unsigned-long))
(define-alien-routine bqn-shape void (a bqnv) (buf (* unsigned-long)))
(define-alien-routine bqn-pick bqnv (a bqnv) (pos unsigned-long))
(define-alien-routine bqn-readi8arr void (a bqnv) (buf (* (signed 8))))
(define-alien-routine bqn-readi16arr void (a bqnv) (buf (* short)))
(define-alien-routine bqn-readi32arr void (a bqnv) (buf (* int)))
(define-alien-routine bqn-readf64arr void (a bqnv) (buf (* double-float)))
;; TODO: L64 pregenerated.rs
