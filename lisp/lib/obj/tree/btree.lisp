;;; btree.lisp --- Lisp B-Trees

;; B-Trees and B+-Trees for Lisp.

;;; Commentary:

;; Mostly useful in DBMS indexing - is the core data structure for some
;; popular K/V stores including RocksDB (B+-tree) and BerkleyDB (B-Tree)
;; [?cite]

;; Originally conceived at Boeing Research Labs in the 70s.

;; https://en.wikipedia.org/wiki/B-tree
;; https://github.com/danlentz/cl-btree
;; https://planetscale.com/blog/btrees-and-database-indexes

;;; Code:
(defpackage :obj/tree/btree
  (:nicknames :obj/btree :btree)
  (:use :cl :std :obj/tree))

(in-package :obj/tree/btree)
