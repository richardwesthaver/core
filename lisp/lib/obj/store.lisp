;;; store.lisp --- Data Store Protocols

;; Support for Lisp Stores.

;;; Commentary:

;; Inspired by Elephant

;; STOREs differ from DBs in that they always prefer transactions over simple
;; set/get.

;;; Code:
(in-package :obj/store)

(defclass store () ())
