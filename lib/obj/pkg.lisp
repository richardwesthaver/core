;;; obj/pkg.lisp --- Object Packages

;;

;;; Code:
(defpackage obj/int
  (:use :cl :std)
  (:export :*obj-packages*))
(in-package :obj/int)

(eval-always (defvar *obj-packages* nil))

(setq *defpkg-hook* (lambda (x) (pushnew (package-name x) *obj-packages* :test 'string=)))

(defpkg :obj/id
  (:nicknames :id)
  (:use :cl :std)
  (:export 
   :id :reset-id :update-id :make-id
   :id-factory))

(defpkg :obj/equiv
  (:nicknames :equiv)
  (:use :cl :std)
  (:export :equiv :eqv :equivalence :equals))

(defpkg :obj/uri/punycode
  (:nicknames :punycode)
  (:use :cl)
  (:export
   :encode-punycode
   :decode-punycode
   :encode-domain
   :decode-domain))

(defpkg :obj/uri
  (:nicknames :uri)
  (:use :cl :std :std/seq :punycode)
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
   :uri-domain
   :uri-condition
   :uri-error))

(pkg:defpkg :obj/url
  (:nicknames :url)
  (:use :cl :std :obj/uri :sb-ext)
  (:export :url-encode :url-decode :url-encode-params :url-decode-params :rewrite-urls))

(defpkg :obj/tensor
  (:nicknames :tensor)
  (:use :cl :std)
  (:export
   #:base-tensor
   #:sparse-tensor
   #:dense-tensor
   #:numeric-tensor
   #:real-numeric-tensor
   #:rational-tensor
   #:fixnum-tensor
   #:standard-tensor
   #:octet-tensor
   #:boolean-tensor
   #:blas-numeric-tensor
   #:real-blas-tensor
   #:real-tensor
   #:sreal-tensor
   #:complex-numeric-tensor
   #:complex-blas-tensor
   #:complex-tensor
   #:scomplex-tensor
   #:real-coordinate-sparse-tensor
   #:real-compressed-sparse-matrix
   #:coordinate-sparse-tensor
   #:compressed-sparse-matrix
   #:*print-tensor-max-len*
   #:*print-tensor-max-args*
   #:*print-tensor-indent*
   #:*tensor-safety-p*
   #:print-tensor
   #:print-element
   #:*default-sparse-store-increment*
   #:*default-sparsity*
   #:*max-sparse-size*
   #:*default-stride-ordering*
   #:*default-tensor-type*
   #:size
   #:store-size
   #:store-ref
   #:subtensor
   #:suptensor
   #:reshape
   #:fc
   #:subfieldp
   #:zeros
   #:%zeros
   #:with-rowm
   #:with-colm
   #:nrows
   #:ncols
   #:row-stride
   #:col-stride
   #:tensor-square-matrixp))

(defpkg :obj/color
  (:nicknames :color)
  (:use :cl :std)
  (:export
   #:color #:alpha
   #:rgb #:red #:green #:blue #:gray
   #:hsv #:hue #:saturation #:.value 
   #:hsi #:intensity
   #:hsl #:lightness 
   #:cmyk #:cyan #:magenta #:yellow #:.key
   #:xyz #:.x #:.y #:.z
   #:lab #:.l #:.a #:.b
   #:rgb-to-hsv #:hsv-to-rgb #:hex-to-rgb #:as-hsv #:as-rgb
   #:rgb-combination #:hsv-combination
   #:parse-hex-rgb #:print-hex-rgb
   :*palette* :palette :base-color-key :find-palette
   :*color-palettes* :base-color-palette-p :get-color :style 
   :make-palette :theme))

(defpkg :obj/time
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
   :timestamp-to-octets
   :duration))

(defpkg :obj/uuid
  (:nicknames :uuid)
  (:use :cl :std :obj/id :obj/time)
  (:export
   :uuid :*ticks-per-count* :format-as-urn :make-null-uuid
   :make-uuid-from-string :make-v1-uuid :make-v3-uuid :make-v4-uuid
   :make-v5-uuid :uuid= :+namespace-dns+ :+namespace-oid+ :+namespace-x500+
   :uuid-to-octet-vector :octet-vector-to-uuid
   :uuid-to-string))

(defpkg :obj/build
  (:nicknames :build)
  (:use :cl :std)
  (:export :build :build-from))

(defpkg :obj/ast
  (:nicknames :ast)
  (:use :cl :std :std/seq)
  (:export :ast 
   :build-ast :load-ast 
   :load-ast* :*ast*
   :wrap :unwrap
   :unwrap-or :form
   :formp :unwrap-object
   :wrap-object :expr
   :literal-expr :unary-expr
   :binary-expr :lhs
   :rhs :physical-expr
   :logical-expr :node
   :defnode :defstmt
   :stmt :defexpr
   :traverse :op
   :*ast-dispatch-table* :write-ast
   :read-ast
   :with-ast :call-with-ast
   :debug-traverser :copy-traverser
   :*keep-ast* :syntax-error
   :syntax-warning :syntax-condition
   :invalid-ast))

(defpkg :obj/graph
  (:nicknames :graph)
  (:use :cl :std :obj/id :ast :std/readtable)
  (:export 
   :vertex :edge :graph :make-edge :make-graph
   :nodes :edges :add-node :add-edge
   :weighted-edge :directed-edge :undirected-edge :directed-graph
   :edge-value :edge-weight :node-edges
   :edgex
   :edge-out
   :edge-in
   :class-graph))

(defpkg :obj/config
  (:nicknames :config)
  (:use :cl :std :ast)
  (:export :config :make-config :find-config
   :config-find :config-get :defconfig
   :load-config))

(defpkg :obj/plan
  (:nicknames :plan)
  (:use :cl :std :obj/ast :obj/config :obj/build)
  (:export :plan :planner
           :logical-plan
           :physical-plan
           :make-physical-plan))

(defpkg :obj/schema
  (:nicknames :schema)
  (:use :cl :std :config :build :meta :stored :sb-mop :id :ast :dynamic :plan)
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
   #:invalid-database
   #:db-condition
   #:apply-schema-change-fn
   #:match-schemas
   #:schema-diff
   #:default-class-constructor
   #:classname
   #:slot-field
   #:slot-field-type
   #:slot-field-args
   #:slot-field-name
   #:slot-field-eq
   #:class-instance-schema
   #:compute-transient-schema
   #:*slot-def-type-tags*
   #:compute-slot-fields
   #:compute-transient-slot-fields
   #:dump-schema
   #:upgradable-schema
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
   #:column-count
   #:make-df
   #:df-col
   #:df
   #:data-frame
   #:schema-from-columns
   #:df-plan))

(defpkg :obj/project
  (:nicknames :project)
  (:use :cl :std :id :schema :plan :config :ast)
  (:export :project :project-config :make-project
   :*default-project-class* :simple-project))

(defpkg :obj/db
  (:nicknames :db)
  (:use :cl :std :id :sb-mop :sb-pcl :schema :dynamic :plan :config)
  (:export
   :get-val
   :set-val
   :ensure-transaction
   :dbs
   :get-db
   :add-db
   :make-db
   :close-db
   :destroy-db
   :connect-db
   :query-db
   :find-db
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
   :remove-kv
   :with-transaction
   :with-batch-transaction
   :*txn*
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
   :db-config
   :*database-collection-type*))

(defpkg :obj/tree
  (:nicknames :tree)
  (:use :cl :std :obj/id)
  (:export :keytype :tree-node :binary-node :unary-node :ternary-node :avl-node
           :make-tree-node :make-binary-node :make-unary-node :make-ternary-node :make-avl-node))

(defpkg :obj/tree/btree
  (:nicknames :obj/btree :btree)
  (:use :cl :std :obj/tree :stored :db)
  (:export
   #:existsp
   #:btree
   #:drop-btree
   #:make-indexed-btree
   #:build-indexed-btree
   #:indexed-btree
   #:build-btree
   #:map-btree
   #:dump-index
   #:btree-keys
   #:remove-current-kv
   #:map-index
   #:with-map-index-collector
   #:iterate-map-index
   #:map-index-values
   #:secondary-cursor
   #:cursor-set-range
   #:cursor-set
   #:cursor-prev
   #:make-cursor
   #:make-simple-cursor
   #:cursor-close
   #:cursor-duplicate
   #:cursor-current
   #:cursor-first
   #:cursor-last
   #:cursor-next
   #:cursor
   #:cursor-get-both
   #:cursor-get-both-range
   #:cursor-delete
   #:cursor-put
   #:cursor-pcurrent
   #:cursor-pfirst
   #:cursor-plast
   #:cursor-pprev
   #:cursor-pset
   #:cursor-pset-range
   #:cursor-pget-both
   #:cursor-pget-both-range
   #:cursor-next-dup
   #:cursor-pnext-dup
   #:cursor-pnext-nodup
   #:cursor-prev-dup
   #:btree-index
   #:build-btree-index
   #:get-primary-key
   #:cursor-initialized-p
   #:cursor-oid
   #:btree-differ-p
   #:print-index-entry
   #:print-btree-key-and-type
   #:dump-btree
   #:print-btree-entry
   #:with-btree-cursor))

(defpkg :obj/secret
  (:nicknames :secret)
  (:use :cl :std)
  (:export :secret :reveal :conceal
   :ensure-revealed :ensure-concealed))

(defpkg :obj/srv
  (:nicknames :srv)
  (:use :cl :std :config :id :ast :build)
  (:export
   #:service
   #:response-ok-p
   #:response-status
   #:dispatch-request
   #:send-response
   #:send-request
   #:receive-response
   #:receive-request
   #:handle-request
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
   #:in-response-p
   #:request
   #:response
   #:service-request-class
   #:service-response-class
   #:engine
   #:restart-service
   #:content-stream
   #:request-protocol
   #:session
   #:service-config
   #:service-request
   #:service-response))

(defpkg :obj/cache
  (:nicknames :cache)
  (:use :cl :std :stored :id :db :schema :config)
  (:import-from :std :queue :make-queue)
  (:export :cache :cache-cleanup :cache-policy
   :cache-size :cache-count :make-cache :cache-release
   :with-cache :cache-remove :cache-flush :cache-entry
   :entry-expiry :entry-weight :make-cache-table :remcache
   :get-cache))

(defpkg :obj/store
  (:nicknames :store)
  (:use :cl :std :stored :sb-mop :meta :btree :id :db :schema :config :cache)
  (:export
   #:store
   #:next-oid
   #:next-cid
   #:*store*
   #:spec
   #:stored-object-schema
   #:temp-spec
   #:delete-spec
   #:copy-spec
   #:optimize-layout
   #:oid->schema-id
   #:default-class-id
   #:default-class-id-type
   #:reserved-oid-p
   #:add-class-store-schema
   #:dropped-instance-p
   #:drop-instance-slots
   #:drop-instance
   #:store-recreate-instance
   #:recreate-instance
   #:recreate-instance-using-class
   #:valid-stored-reference-p
   #:cross-store-error
   #:signal-cross-store-error
   #:with-store
   #:defstore))

(setq *defpkg-hook* nil)
