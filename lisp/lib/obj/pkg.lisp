(defpackage :obj/list
  (:nicknames :list)
  (:use :cl :std)
  (:export :clist))

(defpackage :obj/hash
  (:nicknames :hash)
  (:use :cl :std)
  (:export 
   :*global-hasher*
   :+global-hash+
   :djb
   :hash-object
   :hash-object-address
   :dumb-string-hash
   ;; castable
   :castable
   :make-castable
   :castable-p
   :rehash
   :castable-size
   :castable-count
   :castable-test
   :castable-hasher
   :getchash
   :remchash
   :try-remchash
   :put-if-absent
   :put-if-equal
   :put-if-present
   :clrchash
   :mapchash))

(defpackage :obj/id
  (:nicknames :id)
  (:use :cl :std :obj/hash)
  (:export 
   :id :reset-id :update-id :make-id
   :id-factory))

(defpackage :obj/seq
  (:nicknames :seq)
  (:use :cl :std)
  (:export :iterator :ring))

(defpackage :obj/tree
  (:nicknames :tree)
  (:use :cl :std :obj/id :obj/seq)
  (:export :keytype :node :binary-node :unary-node :ternary-node :avl-node
           :make-node :make-binary-node :make-unary-node :make-ternary-node :make-avl-node))

(defpackage :obj/graph
  (:nicknames :graph)
  (:use :cl :std :obj/id :obj/seq :obj/tree)
  (:export 
   :vertex :edge :graph
   :weighted-edge :directed-edge :undirected-edge))

(defpackage :obj/graph/dot
  (:nicknames :dot)
  (:use :cl :std :obj/graph :obj/id :cli)
  (:export))

(defpackage :obj/color
  (:nicknames :color)
  (:use :cl :std)
  (:export
   #:rgb #:rgb-red #:rgb-green #:rgb-blue #:gray #:&rgb
   #:hsv #:hsv-hue #:hsv-saturation #:hsv-value #:&hsv
   #:rgb-to-hsv #:hsv-to-rgb #:hex-to-rgb #:as-hsv #:as-rgb
   #:rgb-combination #:hsv-combination
   #:parse-hex-rgb #:print-hex-rgb
   :color-palette :parse-and-write-color-definitions))

(defpackage :obj/time
  (:nicknames :time)
  (:use :cl :std)
  (:export
   :timestamp
   :date
   :time-of-day
   :make-timestamp
   :clone-timestamp
   :day-of
   :sec-of
   :nsec-of
   :timestamp<
   :timestamp<=
   :timestamp>
   :timestamp>=
   :timestamp=
   :timestamp/=
   :timestamp-maximum
   :timestamp-minimum
   :adjust-timestamp
   :adjust-timestamp!
   :timestamp-whole-year-difference
   :days-in-month
   :timestamp-
   :timestamp+
   :timestamp-difference
   :timestamp-minimize-part
   :timestamp-maximize-part
   :with-decoded-timestamp
   :decode-timestamp
   :timestamp-century
   :timestamp-day
   :timestamp-day-of-week
   :timestamp-decade
   :timestamp-hour
   :timestamp-microsecond
   :timestamp-millennium
   :timestamp-millisecond
   :timestamp-minute
   :timestamp-month
   :timestamp-second
   :timestamp-week
   :timestamp-year
   :parse-timestring
   :invalid-timestring
   :format-timestring
   :format-rfc1123-timestring
   :to-rfc1123-timestring
   :format-rfc3339-timestring
   :to-rfc3339-timestring
   :encode-timestamp
   :parse-rfc3339-timestring
   :universal-to-timestamp
   :timestamp-to-universal
   :unix-to-timestamp
   :timestamp-to-unix
   :timestamp-subtimezone
   :define-timezone
   :*default-timezone*
   :*clock*
   :leap-second-adjusted
   :clock-now
   :clock-today
   :find-timezone-by-location-name
   :timezones-matching-subzone
   :all-timezones-matching-subzone
   :reread-timezone-repository
   :now
   :today
   :format-date-simple
   :enable-read-macros
   :+utc-zone+
   :+gmt-zone+
   :+month-names+
   :+short-month-names+
   :+day-names+
   :+short-day-names+
   :+seconds-per-day+
   :+seconds-per-hour+
   :+seconds-per-minute+
   :+minutes-per-day+
   :+minutes-per-hour+
   :+hours-per-day+
   :+days-per-week+
   :+months-per-year+
   :+iso-8601-format+
   :+iso-8601-date-format+
   :+iso-8601-time-format+
   :+rfc3339-format+
   :+rfc3339-format/date-only+
   :+asctime-format+
   :+rfc-1123-format+
   :+iso-week-date-format+
   :astronomical-julian-date
   :modified-julian-date
   :astronomical-modified-julian-date
   :zone-name
   :encode-universal-time-with-tz
   :decode-universal-time-with-tz))

(defpackage :obj/music
  (:nicknames :music)
  (:use :cl :std)
  (:export
   :*bpm* :*key-signature* :*time-signature*
   :*chord-table* :*key-table* :*tone-table*))

(defpackage :obj/tbl
  (:nicknames :tbl)
  (:use :cl :std)
  (:import-from :uiop :split-string)
  (:export 
   :table
   :row
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
   :table-from-csv
   :table-from-tvs))

(defpackage :obj/temperature
  (:nicknames :temperature)
  (:use :cl :std)
  (:export :fahrenheit :celsius :kelvin :rankine))

(defpackage :obj/direction
  (:nicknames :direction)
  (:use :cl :std)
  (:export :up :down :left
   :right :east :west :north
   :north-east :north-west :south-east :south-west
   :direction :angle))

(defpackage :obj/shape
  (:nicknames :shape)
  (:use :cl :std)
  (:export :circle :square :cube :sphere :triangle :pyramid))

(defpackage :obj/db
  (:nicknames :db)
  (:use :cl :std :id :seq :sb-mop :sb-pcl)
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
   :make-db
   :close-db
   :destroy-db
   :connect-db
   :query-db
   :db-get
   :close-db
   :database
   :enable-sequences
   :next-sequence
   :sort-docs))

(uiop:define-package :obj
    (:use-reexport :list :hash :color
     :seq :tree :graph :tbl
     :id :db :dot :time
     :music :temperature :direction :shape))
