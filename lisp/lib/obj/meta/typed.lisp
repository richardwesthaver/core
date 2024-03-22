;;; obj/meta/typed.lisp --- Typed meta-objects

;; - typed-slot-class

;; inspired by:
;; https://allegrograph.com/fixed-indices-speed-up-slot-access-in-allegro-cl/

;; may implement fixed.lisp separately.. we'll see.

;;; Commentary:

;; I still need to investigate what the actual behavior is in
;; SBCL.

;; - What sort of type checking is performed on slot-access, when that
;;   slot has type information? Does this vary at different compile levels?

;; - What is the performance impact of injecting additional
;;   slot-accessor type information? For example, declare as
;;   function-type with a typed result.

;;; Code:
(in-package :obj/meta/typed)
