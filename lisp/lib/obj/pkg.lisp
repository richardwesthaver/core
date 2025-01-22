;;; obj/pkg.lisp --- Object System

;;

;;; Code:
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
  (:shadowing-import-from :sb-lockless :endp)
  (:import-from :sb-lockless
   :make-so-map/fixnum :+hash-nbits+
   :node-hash :%node-next
   :unbound-marker-p
   :get-next :node-hash
   :so-head :so-bins
   :so-key :so-data
   :so-count :so-key-node-p
   :so-insert :so-delete
   :so-find :so-find/string
   :so-maplist :make-so-map/string
   :make-so-set/string :make-so-set/fixnum :make-so-map/addr :make-marked-ref
   :make-so-set/addr)
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
  (:export :equiv :eqv :equivalence))

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

(pkg:defpkg :obj/url
  (:nicknames :url)
  (:use :cl :std :obj/uri)
  (:shadowing-import-from :quri :url-encode :url-decode :url-encode-params :url-decode-params)
  (:export :url-encode :url-decode :url-encode-params :url-decode-params))

(defpackage :obj/seq
  (:nicknames :seq)
  (:use :cl :std)
  (:export :iterator :ring
           :next
           :prev
           :iter
           :seek
           :seek-to-first
           :seek-to-last
           :seek-for-prev
           :iter-valid-p
           :*iter*
           :idx
           :with-iter
           :key
           :val))

(defpackage :obj/tree
  (:nicknames :tree)
  (:use :cl :std :obj/id :obj/seq)
  (:export :keytype :tree-node :binary-node :unary-node :ternary-node :avl-node
           :make-node :make-binary-node :make-unary-node :make-ternary-node :make-avl-node))

(defpackage :obj/tree/cursor
  (:nicknames :tree/cursor :cursor)
  (:use :cl :std :obj/id :obj/seq)
  (:export :tree-node :binary-node :unary-node :ternary-node :avl-node
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
   :color-palette :parse-and-write-color-definitions
   :*x11-colors* :*x11-color-palette* :*default-color-palette* :color-palette
   #:make-color-palette
   #:color-palette-p
   #:copy-color-palette
   #:color-palette-table))

(defpackage :obj/time
  (:nicknames :time)
  (:use :cl :std)
  (:export
   :iso-time
   :rfc-1123-date
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
   :decode-universal-time-with-tz
   :octets-to-timestamp
   :timestamp-to-octets))

(defpackage :obj/uuid
  (:nicknames :uuid)
  (:use :cl :std :obj/id :obj/time)
  (:export
   :uuid :*ticks-per-count* :format-as-urn :make-null-uuid
   :make-uuid-from-string :make-v1-uuid :make-v3-uuid :make-v4-uuid
   :make-v5-uuid :uuid= :+namespace-dns+ :+namespace-oid+ :+namespace-x500+
   :uuid-to-octet-vector :octet-vector-to-uuid))

(defpackage :obj/unit
  (:nicknames :unit)
  (:use :cl :std)
  (:export :up :down :left
   :right :east :west :north
   :north-east :north-west :south-east :south-west
   :direction :angle :fahrenheit :celsius :kelvin :rankine))

(defpackage :obj/build
  (:nicknames :build)
  (:use :cl :std)
  (:export :build :build-from))

(defpackage :obj/ast
  (:nicknames :ast)
  (:use :cl :std)
  (:shadowing-import-from :obj/seq :val)
  (:export :ast :build-ast :load-ast :load-ast*
           :val
           :*ast*
           :wrap
           :unwrap
           :unwrap-or
           :form
           :formp
           :unwrap-object
           :wrap-object
           :expr
           :literal-expr
           :unary-expr
           :binary-expr
           :lhs
           :rhs
           :physical-expr
           :logical-expr
           :node
           :defnode
           :defstmt
           :stmt
           :defexpr
           :traverse
           :op
           :*ast-dispatch-table*
           :write-ast
           :read-ast
           :val
           :debug-traverser
           :copy-traverser
           :*keep-ast*
           :syntax-error
           :syntax-warning
           :syntax-condition))

(defpackage :obj/config
  (:nicknames :config)
  (:use :cl :std :ast)
  (:export :config :make-config :find-config
   :config-find :config-get :defconfig
   :load-config))

(defpackage :obj/schema
  (:nicknames :schema)
  (:use :cl :std :config :build :meta :stored :sb-mop :id :ast :dynamic)
  (:export
   #:schema
   #:object-schema
   #:simple-schema
   #:schema-metadata
   #:field-vector
   #:field
   #:load-field
   #:load-schema
   #:derive-schema
   #:fields
   #:make-simple-schema
   #:make-field
   #:field-p
   #:copy-field
   #:field-name
   #:field-type
   #:defschema
   #:list-to-fields
   #:define-simple-schema
   #:not-a-database
   #:db-condition
   #:match-schemas
   #:schema-diff
   #:default-class-constructor
   #:classname
   #:slot-field
   :slot-field-type
   :slot-field-args
   :slot-field-name
   #:slot-field-eq
   #:class-instance-schema
   #:compute-transient-schema
   #:*slot-def-type-tags*
   #:compute-slot-fields
   #:compute-transient-slot-fields
   #:dump-schema
   #:dump-slots
   #:sorted-slots
   #:slot-defs-from-schema
   #:make-fields
   #:schema-class-name
   #:schema-successor
   #:schema-predecessor
   #:diff-type
   #:diff-recs
   #:apply-schema
   #:dynamic-schema
   #:literal-value-vector
   #:column-vector
   #:literal-value-type
   #:*literal-value-types*
   #:column-literal-value
   #:column-type
   #:column-value
   #:column-size
   #:scan-data
   #:record-batch
   #:record-batch-schema
   #:record-batch-fields
   #:make-record-batch
   #:data-source
   #:file-data-source
   #:columns
   #:column
   #:cons-column
   #:simple-column
   #:simple-cons-column
   #:make-schema
   #:row-count
   #:column-count))

(defpackage :obj/plan
  (:nicknames :plan)
  (:use :cl :std :obj/ast :obj/config :obj/build)
  (:export :plan :planner
           :logical-plan
           :physical-plan
           :make-physical-plan))

(defpackage :obj/query
  (:nicknames :query)
  (:use :cl :std :plan :ast :schema)
  (:export :query
           :query-expression
           :logical-expression
           :column-expression
           :literal-expression
           :row-count
           :column-count
           :record-batch
           :make-query
           :*literal-value-types*
           :literal-value-type
           :literal-value-vector
           :projection
           :selection
           :aggregate
           :data-frame
           :execution-context
           :physical-expression
           :scan-exec
           :scan-data
           :execute-query
           :aggregate-function
           :aggregate-function-designator
           :aggregate-expression
           :binary-expression
           :unary-expression
           :alias-expression
           :query-optimizer
           :make-physical-expression
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
           :evaluate
           :make-record-batch
           :record-batch-p
           :copy-record-batch
           :record-batch-schema
           :record-batch-fields
           :column-size
           :column-value
           :column-type
           :column-vector
           :column-data
           :math-expression
           :add-expression
           :sub-expression
           :mult-expression
           :div-expression
           :mod-expression
           :and-expression
           :or-expression
           :lteq-expression
           :gteq-expression
           :lt-expression
           :gt-expression
           :neq-expression
           :eq-expression
           :aggregate-expression-p
           :df-col
           :df-project
           :df-filter
           :df-aggregate
           :df-select
           :df-fields
           :df-data
           :limit
           :make-df
           :binary-expression-name
           :binary-expression-op
           :sum-expression
           :min-expression
           :max-expression
           :avg-expression
           :count-expression
           :to-field
           :column-name
           :cast-expression
           :df-plan
           :df-exec
           :execute*
           :register-file
           :register-data-source
           :register-df
           :file-data-path
           :optimize-query
           :projection-pushdown-optimizer
           :extract-columns*
           :extract-columns
           :query-vop
           :logical-query-plan
           :physical-query-plan
           :query-plan
           :query-expr
           :project
           :select
           :boolean-binary-expression))

(defpackage :obj/db
  (:nicknames :db)
  (:use :cl :std :id :seq :sb-mop :sb-pcl :schema :dynamic :query :plan :config)
  (:export
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
   :database
   :db-closed-p
   :db-open-p
   :*db*
   :get-value
   :insert-key
   :insert-kv
   :make-kv
   :delete-key
   :delete-key-ts
   :delete-key-range
   :make-transaction
   :prepare-transaction
   :rollback-transaction
   :commit-transaction
   :flush-db
   :sync-db
   :repair-db
   :backup-db
   :restore-db
   :snapshot-db
   :write-db
   :shutdown-db
   :ingest-db
   :put-kv
   :put-key
   :put-key-ts
   :get-key
   :multi-get
   :execute-transaction
   :abort-transaction
   :kv
   :make-val
   :make-key
   :open-db
   :kv-key
   :kv-val
   :database-collection
   :upgradable-schema
   :upgrade
   :version
   :remove-kv
   :with-transaction
   :with-batch-transaction
   :*txn*
   :apply-schema-change-fn
   :transaction-object
   :current-transaction
   :transaction-store
   :database-version
   :transaction-db
   :transaction-object-p
   :known-transaction
   :close-column
   :close-columns
   :find-column
   :flush-column
   :transaction-prior
   :add-column
   :open-columns
   :merge-kv
   :merge-key
   :db-stats
   :db-metadata
   :db-prop
   :db-opt
   :create-columns
   :set-db-opt
   :with-temp-db
   :with-db
   :database-backend-designator
   :add-database-loader
   :*database-backend-table*
   :load-database-backend
   :*database-backend-options*
   :*database-backend*
   :add-database-options
   :set-database-loaders
   :add-database-backend-option
   :set-database-backend-options
   :do-database-backend-init-options
   :do-database-backend-close-options
   :set-database-backend
   :set-database-backend-option
   :column-opts
   :transaction-opts
   :db-lock
   :simple-transaction
   :secondary-db
   :db-backup
   :db-opts
   :*save-database-backend-on-load*
   :open-with-columns
   :open-columns*
   :open-column
   :destroy-column
   :destroy-columns
   :create-column
   :db-config))

(defpackage :obj/secret
  (:nicknames :secret)
  (:use :cl :std)
  (:export :secret :reveal :conceal
   :ensure-revealed :ensure-concealed))

(defpackage :obj/srv
  (:nicknames :srv)
  (:use :cl :std)
  (:export
   #:response-ok-p
   #:response-status
   #:dispatch-request
   #:send-response
   #:handle-request
   #:process-request
   #:service
   #:bad-request
   #:simple-service-warning
   #:service-warning
   #:simple-service-error
   #:service-error
   #:service-condition
   #:*service*
   #:*service-table*
   #:*request*
   #:*response*
   #:in-request-p
   #:in-response-p))
