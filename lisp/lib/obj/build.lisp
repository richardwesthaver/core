;;; obj/build.lisp --- Builder API

;; BUILDER class and methods.

;;; Commentary:

;; This package started during the implementation of URING when it became
;; clear that we needed a generic 'CONS-like' protocol and class for objects
;; capable of constructing complex structures.

;;  NOTE 2024-10-26: also to be used in DAT/TAR

;;; Code:
(in-package :obj/build)

(defclass builder () ())

(defgeneric build (self &key &allow-other-keys))
(defgeneric build-from (self from &key &allow-other-keys))
