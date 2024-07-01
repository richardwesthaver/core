;;; obj/pkg.lisp --- Object System

;;

;;; Code:
(defpackage :obj/meta
  (:nicknames :meta)
  (:use :cl :std)
  (:export
   :class-equalp
   :*standard-metaobjects*))

(defpackage :obj/list
  (:nicknames :list)
  (:use :cl :std)
  (:import-from :sb-lockless
   :make-ordered-list :lfl-insert
   :lfl-delete :lfl-find
   :lfl-insert*/t :lfl-delete*/t :lfl-find*/t
   :do-lockfree-list :lfl-keys :make-marked-ref)
  (:export :clist))

(defpackage :obj/hash
  (:nicknames :hash)
  (:use :cl :std)
  (:import-from :sb-lockless
   :make-so-map/fixnum :+hash-nbits+
   :get-next :node-hash
   :so-head :so-bins
   :so-key :so-data
   :so-count :so-key-node-p
   :so-insert :so-delete
   :so-find :so-find/string
   :so-maplist :make-so-map/string
   :make-so-set/string :make-so-map/addr :make-marked-ref)
  (:export 
   :*global-hasher*
   :*global-hash*
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

(defpackage :obj/equiv
  (:use :cl :std)
  (:export :equiv :eqv :nequiv :neqv :equivalence))

(defpackage :obj/uri
  (:nicknames :uri)
  (:use :cl :std)
  (:export
   :uri				; class
   :uri-p
   :iri				; subclass of uri
   :iri-p
   :copy-uri
   
   :uri-parse-error
   :uri-parse-error-string

   :uri-scheme
   :uri-userinfo
   :uri-port
   :uri-path
   :uri-query
   :uri-fragment
   :uri-ipv6
   :uri-zone-id
   :uri-plist
   :uri-authority			; pseudo-slot accessor
   :uri-host
   :urn				; class
   :urn-nid
   :urn-nss
   :urn-q-component			; RFC 8141
   :urn-f-component			; RFC 8141
   :urn-r-component			; RFC 8141
   :*strict-parse*
   :parse-uri
   :merge-uris
   :enough-uri
   :uri-parsed-path
   :render-uri
   :string-to-uri
   :uri-to-string
   :string-to-iri
   :iri-to-string
   :parse-uri-string-rfc3986
   :parse-iri-string-rfc3987
   :make-uri-space			; interning...
   :uri-space
   :uri=
   :intern-uri
   :unintern-uri
   :do-all-uris
   :uri-to-pathname
   :pathname-to-uri
   ;; domains
   :parse-domain
   :ipv4-addr-p
   :ipv6-addr-p
   :ip-addr-p
   :ip-addr=
   :uri-tld
   :uri-domain))

(defpackage :obj/url
  (:nicknames :url)
  (:use :cl :std :obj/uri)
  (:import-from :quri :url-encode :url-decode :url-encode-params :url-decode-params))

(defpackage :obj/seq
  (:nicknames :seq)
  (:use :cl :std)
  (:export :iterator :ring))

(defpackage :obj/tree
  (:nicknames :tree)
  (:use :cl :std :obj/id :obj/seq)
  (:export :keytype :tree-node :binary-node :unary-node :ternary-node :avl-node
           :make-node :make-binary-node :make-unary-node :make-ternary-node :make-avl-node))

(defpackage :obj/graph
  (:nicknames :graph)
  (:use :cl :std :obj/id :obj/seq)
  (:export 
   :vertex :edge :graph :make-edge :make-graph
   :nodes :edges :add-node :add-edge
   :weighted-edge :directed-edge :undirected-edge :directed-graph
   :edge-value :edge-weight :node-edges))

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

(defpackage :obj/uuid
  (:nicknames :uuid)
  (:use :cl :std :obj/id :obj/time)
  (:export
   :uuid :*ticks-per-count* :format-as-urn :make-null-uuid
   :make-uuid-from-string :make-v1-uuid :make-v3-uuid :make-v4-uuid
   :make-v5-uuid :uuid= :+namespace-dns+ :+namespace-oid+ :+namespace-x500+
   :uuid-to-octet-vector :octet-vector-to-uuid))

(defpackage :obj/music
  (:nicknames :music)
  (:use :cl :std)
  (:export
   :*bpm* :*key-signature* :*time-signature*
   :*chord-table* :*key-table* :*tone-table*))

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

(defpackage :obj/cfg
  (:nicknames :cfg)
  (:use :cl :std)
  (:export :cfg :make-cfg :find-cfg
   :cfg-find :cfg-get :defcfg))

(defpackage :obj/db
  (:nicknames :db)
  (:use :cl :std :id :seq :sb-mop :sb-pcl)
  (:export
   :load-db
   :get-val
   :set-val
   :dbs
   :get-db
   :add-db
   :make-db
   :close-db
   :destroy-db
   :connect-db
   :query-db
   :db-get
   :db
   :database))

(defpackage :obj/query
  (:nicknames :query)
  (:use :cl :std)
  (:export :query
           :data-source
           :query-expression
           :logical-expression
           :column-expression
           :literal-expression
           :field
           :fields
           :row-count
           :column-count
           :record-batch
           :schema
           :derive-schema
           :load-schema
           :make-schema
           :make-query
           :field-vector
           :*literal-value-types*
           :literal-value-type
           :literal-value-vector
           :projection
           :selection
           :aggregate
           :data-frame
           :execution-context
           :physical-expression
           :physical-plan
           :scan-exec
           :scan
           :execute-query
           :aggregate-function
           :aggregate-function-designator
           :aggregate-expression
           :binary-expression
           :unary-expression
           :alias-expression
           :query-optimizer
           :create-physical-plan
           :create-physical-expression
           :query-planner
           :hash-aggregate-exec
           :filter
           :selection-exec
           :projection-exec
           :execute
           :max-physical-expression
           :aggregate-physical-expression
           :accumulated
           :accumulate
           :accumulator
           :math-physical-expression
           :equiv-physical-expression
           :binary-physical-expression
           :literal-physical-expression
           :column-physical-expression
           :evaluate))

(defpackage :obj/secret
  (:nicknames :secret)
  (:use :cl :std)
  (:export :secret-object :reveal-object :conceal-object
   :ensure-revealed :ensure-concealed))

(defpackage :obj/build
  (:use :cl :std)
  (:export :build :build-from))

(uiop:define-package :obj
  (:use-reexport :list :hash :color
   :seq :tree :graph :id
   :db :time :uri :url
   :cfg :music :temperature :direction
   :shape :secret :query))
