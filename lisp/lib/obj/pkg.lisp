(defpackage :obj/hash
  (:nicknames :hash)
  (:use :cl :std)
  (:export 
   :hash-object))

(defpackage :obj/id
  (:nicknames :id)
  (:use :cl :std :obj/hash)
  (:export 
   :id :reset-id :update-id :make-id))

(defpackage :obj/seq
  (:nicknames :seq)
  (:use :cl :std)
  (:export :iterator :ring))

(defpackage :obj/tree
  (:nicknames :tree)
  (:use :cl :std :obj/id :obj/seq)
  (:export :node :binary-node :unary-node))

(defpackage :obj/graph
  (:nicknames :graph)
  (:use :cl :std :obj/id :obj/seq :obj/tree)
  (:export 
   :vertex :edge :graph
   :weighted-edge :directed-edge :undirected-edge))

(defpackage :obj/color
  (:nicknames :color)
  (:use :cl :std)
  (:export
   #:rgb #:rgb-red #:rgb-green #:rgb-blue #:gray #:&rgb
   #:hsv #:hsv-hue #:hsv-saturation #:hsv-value #:&hsv
   #:rgb-to-hsv #:hsv-to-rgb #:hex-to-rgb #:as-hsv #:as-rgb
   #:rgb-combination #:hsv-combination
   #:parse-hex-rgb #:print-hex-rgb))

(defpackage :obj/tbl
  (:nicknames :tbl)
  (:use :cl :std)
  (:import-from :uiop :split-string)
  (:export 
   :make-table
   :make-row
   :add-to-table
   :add-to-row
   :get-row
   :get-row-column
   :set-row-column
   :num-rows
   :num-cols
   :num-col
   :rectangular-table-p
   :sequence->row
   :row-sequence->table
   :with-rows
   :select
   :distinct
   :top
   :order-by
   :where
   :where-filter
   :where-or
   :where-and
   :read-csv
   :read-tsv
   :table-from-file))

(defpackage :obj/db
  (:nicknames :db)
  (:use :cl :std :id :seq :sb-mop)
  (:export
   :xdb
   :collection
   :collection-aware
   :map-docs
   :duplicate-doc-p
   :find-duplicate-doc
   :store-doc
   :serialize-doc
   :serialize-docs
   :load-from-file
   :get-collection
   :add-collection
   :snapshot
   :load-db
   :get-docs
   :get-doc
   :get-val
   :set-val
   :sum
   :max-val
   :document
   :doc-type
   :key
   :find-doc
   :find-docs
   :sort-collection
   :docs
   :*fsync-data*
   :storable-class
   :dbs
   :get-db
   :add-db
   :enable-sequences
   :next-sequence
   :sort-docs))

(uiop:define-package :obj
  (:use-reexport :hash :color :seq :tree :graph :tbl :id :db))
