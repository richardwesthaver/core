;;; obj/pkg.lisp --- Object Packages

;; A collection of [[id:8bc4ff14-f001-4515-ac1f-fd59ef8ef506][CLOS]]-based protocols.

;;; Commentary:

;; Each package in the =obj= system is loosely speaking its own
;; 'protocol', providing various classes and generic functions.

;; There is a special category of package prefixed with =obj/meta/= -
;; these packages operate on the level of the [[id:f9d8eead-51ab-4589-a5c2-2569ea1b7abd][Meta-Object Protocol]] and
;; provide special classes called /metaclasses/.

;;; Code:
(defpkg :obj/val
  (:nicknames :val)
  (:use :cl :std)
  (:export :get-val :rem-val :get-value))

;;; Meta Packages
(defpkg :obj/meta/stealth
  (:nicknames :meta/stealth :stealth)
  (:use :cl :std :sb-mop)
  (:export
   #:add-mixin
   #:define-stealth-mixin))

(defpkg :obj/meta/filtered
  (:nicknames :meta/filtered :filtered)
  (:use :cl :std :sb-mop)
  (:export
   :define-filtered-function :filtered :filtered-function :filtered-method
   :generic-function-filter-expression :generic-function-filters :method-filter :simple-filtered-function))

(defpkg :obj/meta/sealed
  (:nicknames :meta/sealed :sealed)
  (:use :cl :std)
  (:import-from :sb-pcl :eql-specializer :intern-eql-specializer
   :eql-specializer-object :funcallable-standard-class)
  (:import-from :sb-mop :class-finalized-p :finalize-inheritance
   :class-precedence-list :class-direct-superclasses :specializer :method-specializers
   :generic-function-argument-precedence-order :generic-function-name :generic-function-methods :class-direct-subclasses
   :class-prototype)
  (:export
   :ensure-specializer
   :specializer-type
   :specializer-prototype
   :specializer-direct-superspecializers
   :specializer-intersectionp
   :specializer-subsetp
   :domain
   :ensure-domain
   :method-domain
   :domain-specializers
   :domain-arity
   :domain-equal
   :domain-intersectionp
   :domain-subsetp

   :metaobject-sealable-p
   :class-sealable-p
   :generic-function-sealable-p
   :method-sealable-p
   :specializer-sealable-p

   :metaobject-sealed-p
   :class-sealed-p
   :generic-function-sealed-p
   :method-sealed-p
   :specializer-sealed-p

   :seal-class
   :seal-generic-function
   :seal-method
   :seal-domain
   :seal-specializer

   :method-properties
   :validate-method-property

   :static-call-signature
   :static-call-signature-types
   :static-call-signature-prototypes

   :sealed-domains
   :compute-static-call-signatures
   :externalizable-object-p
   :sealable-class
   :sealable-generic-function
   :sealable-standard-generic-function
   :potentially-sealable-method
   :potentially-sealable-standard-method))

(defpkg :obj/meta/fast
  (:nicknames :meta/fast :fast)
  (:use :cl :std :obj/meta/sealed)
  (:import-from :sb-int :gensymify)
  (:import-from :sb-walker :macroexpand-all)
  (:export :fast-generic-function :fast-method :inlineable :.lambda.))

(defpkg :obj/meta/lazy
  (:nicknames :meta/lazy :lazy)
  (:use :cl :std))

(defpkg :obj/meta/overload
  (:nicknames :meta/overload :overload)
  (:use :cl :std))

(defpkg :obj/meta/stored
  (:documentation "Stored object definitions.
This package defines the generic MOP machinery which drives the STORE
protocol.")
  (:nicknames :meta/stored :stored)
  (:use :cl :std :sb-mop :val)
  (:export
   :*store*
   :*store-table*
   :*default-store*
   :stored-class :initialize-stored-class
   :stored-slot
   :stored
   :stored-object
   :stored-collection
   :spec
   :stored-p
   :indexed-slot-names
   :indexed-slot-defs
   :stored-slot-definition
   :indexed-slot-definition
   :derived-index-slot-definition
   :derived-index-effective-slot-definition
   :derived-index-direct-slot-definition
   :derived-slot-triggers
   :derived-fn
   :derived-index-slot-names
   :get-slot-def-index
   :add-slot-def-index
   :clear-slot-def-index
   :indexed-slot-base
   :indexed-slot-indices
   :get-store-schemas
   :get-class-indexing
   :get-cache-style
   :cached-slot-defs
   :find-slot-defs-by-type
   :migrate-class-index-p
   :class-indexing-enabled-p
   :defsclass
   :get-class-schema
   :drop-instance
   :register-instance
   :cache-instance
   :get-cached-instance
   :uncache-instance
   :flush-instance-cache
   :stored-slot-makunbound
   :stored-slot-boundp
   :stored-slot-writer
   :stored-slot-reader
   :get-store
   :read-oid
   :write-oid
   :stored-slot-names
   :all-stored-slot-names
   :all-single-valued-slot-defs
   :cached-slot-definition
   :cached-direct-slot-definition
   :transient-slot-definition
   :cached-slot-names
   :transient-p
   :transient-slot-names
   :database-allocation-p
   :slot-definition-allocation
   :association-slot-base
   :association-type
   :association-effective-slot-definition
   :association-slot-definition
   :association-slot-indices
   :foreign-classname
   :foreign-slotname
   :foreign-class
   :association-end-p
   :association-slot-defs
   :association-slot-names
   :association-end-slot-names
   :get-association-slot-index
   :add-association-slot-index
   :remove-association-slot-index
   :set-valued-slot-names
   :set-valued-slot-definition
   :set-valued-direct-slot-definition
   :set-valued-effective-slot-definition))

(defpkg :obj/meta/dynamic
  (:nicknames :meta/dynamic :dynamic)
  (:use :cl :std :std/macs)
  (:export :dset :dref :dynamic-class
   :slot-dlet :slot-dvar :slot-dvar*))

(defpkg :obj/meta/mix
  (:use #:cl #:std)
  (:export 
   #:mixin-class #:mixin-object #:mixin-classes
   #:ensure-mix #:delete-from-mix #:mix
   #:replace-class #:replace-class-in-mixin
   #:set-mix-rule #:*class-ordering-rules*
   #:%find-class
   #:ensure-mixin
   #:make-mix-list
   #:mix-list))

(defpkg :obj/meta/method
  (:use :cl :std))

(defpkg :obj/equiv
  (:nicknames :equiv)
  (:use :cl :std)
  (:export :equiv :eqv :equivalence :equals))

(defpkg :obj/id
  (:nicknames :id)
  (:use :cl :std :equiv)
  (:export 
   :id :reset-id :update-id :make-id
   :identifier :*global-id-table* :global-id :global-id-value
   :update-global-id :reset-global-id :identify :id=
   :next-id))

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
  (:use :std-lisp :punycode)
  (:export
   :uri
   :uri-p
   :iri
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
   :uri-authority
   :uri-host
   :urn
   :urn-nid
   :urn-nss
   :urn-q-component
   :urn-f-component
   :urn-r-component
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
   :make-uri-space
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
  (:use :std-lisp :obj/uri :sb-ext)
  (:export :url-encode :url-decode :url-encode-params :url-decode-params :rewrite-urls))

(defpkg :obj/tensor
  (:nicknames :tensor)
  (:use :std-lisp)
  (:export :tensor 
   :foreign-dense-tensor :foreign-tensor 
   :blas-mixin #:orphanize 
   #:index-type #:index-store-vector
   #:dimensions #:order #:field-type #:ref #:einstein-sum
   #:base-tensor #:memos #:store-size #:total-size 
   #:store-ref #:store-type
   #:parent #:dorefs
   #:print-tensor
   #:tensor-class #:sparse-tensor #:stride-tensor #:dense-tensor #:simple-dense-tensor #:foreign-dense-tensor
   #:graph-accessor #:stride-accessor #:base-accessor #:coordinate-accessor
   #:hash-tensor #:graph-tensor 
   #:coordinate-tensor
   #:with-coordinates
   #:tensor-typep #:tensor-type
   #:tensor-method-generator #:define-tensor-method #:cl #:zeros
   #:define-tensor-generic
   #:deftensor #:tensor-dimension-mismatch
   #:with-field-element #:clinear-storep #:complexified-tensor #:blas-func
   #:with-field-elements #:with-columnification #:without-tensor-safety
   #:call-alien-p #:tensor-matrixp #:split-job #:cclass-max
   #:*default-uplo* #:*default-tensor-type*
   #:*default-stride-ordering*
   #:tensor-vector #:tensor-matrix #:tensor-square-matrix
   #:complexified-tensor #:realified-tensor
   #:real-subtypep #:tensor-vectorp
   #:field-realp
   #:indices #:fence #:δ-I #:strides #:head #:store
   #:idxv #:pick-random #:shuffle! #:permutation #:permutation-action #:permutation-cycle
   #:permutation-pivot-flip #:permute! #:permute #:permutation/ #:permutation* #:permutation-size
   #:sort-permute #:subtensor~ #:slice~ #:suptensor~ #:reshape! #:reshape~
   #:matrixify~ #:join #:minors
   #:tensor-realpart~ #:tensor-realpart #:tensor-imagpart~ #:tensor-imagpart
   #:transpose! #:transpose~ #:transpose #:ctranspose! #:ctranspose #:tensor-conjugate! #:tensor-conjugate
   #:t.fid+ #:t.f+ #:t.f* #:t.fc 
   #:t.blas-threshold #:t.store
   #:t.f= #:t.store-type #:t.store-allocator #:t.store-ref
   #:t.strict-coerce
   ;;L1
   #:copy! #:tensor-copy #:swap! #:swap #:dot #:scal! #:scal #:div! #:div #:scald!
   #:tricopy!
   ;; bool
   #:ge= #:ga= #:go=
   #:mapsor! #:mapsor #:map-tensor! #:mapslice #:mapslice~ #:mapslicec~ #:tensor-foldl
   #:ones #:eye! #:eye #:diag #:diag~
   #:rand #:randn #:randi #:rande
   #:generate-rand
   #:range #:linspace
   #:einstein-sum))

(defpkg :obj/ast
  (:nicknames :ast)
  (:use :std-lisp)
  (:export :ast 
   :load-ast
   :wrap :unwrap
   :form
   :formp :unwrap-object
   :wrap-object :expr
   :literal-expr :unary-expr
   :binary-expr :lhs
   :rhs :physical-expr
   :logical-expr :node
   :defnode :defstmt
   :stmt :defexpr
   :traverse :op
   :write-ast
   :read-ast :with-ast :call-with-ast :invalid-ast
   :debug-traverser :copy-traverser
   :*keep-ast* :syntax-error
   :syntax-warning :syntax-condition
   :*ast* :document :with-object-ast))

(defpkg :obj/config
  (:nicknames :config)
  (:use :cl :std :ast)
  (:export :config :make-config :find-config
   :config-find :config-get :defconfig :load-config))

(defpkg :obj/color
  (:nicknames :color)
  (:use :cl :std :config :equiv)
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
   #:colors
   :*palette* :palette :base-color-key :find-palette
   :*color-palettes* :base-color-palette-p :get-color :style 
   :make-palette :theme :with-palette :call-with-palette
   :remove-palette :*theme* :find-theme :load-theme 
   :deftheme :load-palette
   :missing-palette :color-error
   ;; color-table
   :+max-color-table-size+
   :color-rgb :rgb-color :make-color-table :color-table-code-size
   :find-color :add-color :ensure-color :copy-color-table
   :color-table-full :missing-color-table :color-table
   :rgb-color*))

(defpkg :obj/time
  (:nicknames :time)
  (:use :cl :std)
  (:export
   :iso-time
   :unix-time
   :real-time
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
   :duration
   :timetag
   :timetag+
   :unix-time-to-timetag
   :timetag-to-unix-time))

(defpkg :obj/uuid
  (:nicknames :uuid)
  (:use :cl :std :obj/id :obj/time)
  (:export
   :uuid :*ticks-per-count* :format-as-urn :make-null-uuid
   :make-uuid-from-string :make-v1-uuid :make-v3-uuid :make-v4-uuid
   :make-v5-uuid :uuid= :+namespace-dns+ :+namespace-oid+ :+namespace-x500+
   :uuid-to-octet-vector :octet-vector-to-uuid
   :uuid-to-string))

(defpkg :obj/schema
  (:nicknames :schema)
  (:import-from :obj/tensor :field-type)
  (:use :cl :std :config :stored :sb-mop :id :ast :dynamic)
  (:export
   #:schema
   #:ensure-schemas
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
   #:field-type
   #:defschema
   #:list-to-fields
   #:define-simple-schema
   #:invalid-database
   #:db-condition
   #:db-error
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
   ;; #:dynamic-schema
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

(defpkg :obj/cmd
  (:nicknames :cmd :command)
  (:use :cl :std :ast)
  (:export :defcommand
   :interactive :define-command-type
   :*commands* :*command-table*
   :*command* :*command-class*
   :*command-types* :command-type
   :command-table :command
   :*interactive-optional-args-p*
   :*interactive-rest-args-p*
   :*interactive-key-args-p*
   :with-commands :read-command
   :write-command :parse-command
   :save-commands :copy-commands
   :fmt-command :call-interactively
   :interactive-required-count
   :interactive-total-count
   :icount :icount*
   :map-commands
   :map-command-types
   :parse-command* :read-command*
   :commandp :commands
   :list-commands :list-command-types
   :command-types :make-commands
   :command-alias :load-commands
   :run-commands :print-help
   :print-usage :call-command
   :cmd :*command-hook*
   :eval-command :parse-args
   :read-arg :read-args
   :parse-interactive-lambda-list :*command-io*
   :command-class :*command-names-p*
   :list-all-commands :*commander*
   :print-command :command-name
   :command-eval-error :undefined-command
   :undefined-command-type :invalid-command-type
   :invalid-itype :command-error
   :command-warning))

(defpkg :obj/project
  (:nicknames :project)
  (:use :cl :std :id :schema :config :ast :cmd :url)
  (:export :project :project-config :make-project
   :*default-project-class* :simple-project :project-metadata :author
   :version :tags :description :license
   :rule :simple-rule :interactive-rule :project-compile
   :project-load :project-component :project-module :with-project
   :with-rule :*rule* :*project* :project-convert
   :load-project-component :project-find :project-config-slot :search-project
   :project-slot :*project-config* :make-rule :rules
   :*default-rule-class* :*project-hook* :rule-target :simple-interactive-rule
   :links :project-link :project-tag-p :list-all-projects
   :defproject :*project-env* :find-project :register-project
   :project-paths :project-directories :project-root))

(defpkg :obj/db
  (:nicknames :db)
  (:import-from :obj/meta/stored :*store*)
  (:use :cl :std :id :sb-mop :sb-pcl :schema :dynamic :config :ast)
  (:export
   :ensure-transaction
   :dbs
   :get-db
   :add-db
   :make-db
   :close-db
   :destroy-db
   :connect-db
   :find-db
   :db
   :database
   :db-closed-p
   :db-open-p
   :*db*
   :insert-key
   :delete-key
   :prepare
   :rollback
   :commit
   :flush-db
   :repair-db
   :backup-db
   :restore-db
   :snapshot-db
   :shutdown-db
   :ingest-db
   :put-key
   :get-key
   :multi-get
   :abort-transaction
   :open-db
   :remove-kv
   :with-transaction
   :transaction-error
   :with-batch-transaction
   :*transaction*
   :transaction-object
   :transaction
   :current-transaction
   :transaction-store
   :database-version
   :transaction-db
   :transaction-object-p
   :known-transaction
   :close-columns
   :find-column
   :transaction-prior
   :add-column
   :open-columns
   :merge-key
   :db-stats
   :db-metadata
   :create-columns
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
   :simple-transaction
   :secondary-db
   :db-backup
   :*save-database-backend-on-load*
   :open-with-columns
   :open-columns*
   :open-column
   :close-columns
   :create-column
   :db-config
   :simple-db-config))

(defpkg :obj/graph
  (:nicknames :graph)
  (:use :cl :std :obj/id :ast :std/readtable :obj/equiv)
  (:import-from :obj/val :get-val)
  (:export 
   :vertex :edge :make-edge :make-graph
   :nodes :edges :add-node :add-edge
   :weighted-edge :directed-edge :edge-value :edge-weight
   :edge-weight :node-edges :edgex :edge-out
   :edge-in :subgraph :delete-node :merge-nodes :merge-edges :degree
   :graph
   :simple-graph
   :directed-graph
   :simple-directed-graph
   :class-graph
   :weight
   :indegree :outdegree
   :shortest-path :min-cut))

(defpkg :obj/node
  (:nicknames :node)
  (:use :cl :std :obj/id)
  (:export :keytype :tree-node :binary-node :unary-node :ternary-node :avl-node
           :make-tree-node :make-binary-node :make-unary-node :make-ternary-node :make-avl-node))

(defpkg :obj/btree
  (:nicknames :btree)
  (:use :cl :std :node :stored :db :val)
  (:export
   #:primary
   #:key-form
   #:key-fn
   #:existsp
   #:btree
   #:drop-btree
   #:make-indexed-btree
   #:make-btree
   #:dup-btree
   #:indexed-btree
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
   #:make-btree-index
   #:get-primary-key
   #:cursor-initialized-p
   #:cursor-oid
   #:btree-differ-p
   #:print-index-entry
   #:print-btree-key-and-type
   #:dump-btree
   #:print-btree-entry
   #:with-btree-cursor
   #:make-dup-btree
   #:*cursor*
   #:compare-equal
   #:compare>=
   #:compare<
   #:compare<=))

(defpkg :obj/secret
  (:nicknames :secret)
  (:use :cl :std)
  (:export :secret :reveal :conceal
   :ensure-revealed :ensure-concealed))

(defpkg :obj/srv
  (:nicknames :srv)
  (:use :cl :std :config :id :ast)
  (:export
   #:defservice
   #:with-service
   #:single-threaded-engine
   #:multi-threaded-engine
   #:accept
   #:service
   #:find-service
   #:list-all-services
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
   #:register-service
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
  (:use :cl :std :stored :id :db :schema :config :val)
  (:import-from :std :queue :make-queue)
  (:import-from :graph :weight)
  (:export :cache 
   :cache-cleanup :cache-policy
   :cache-size :cache-count 
   :make-cache :cache-fetch
   :with-cache :cache-remove 
   :cache-flush :cache-entry
   :make-cache-table :get-cache 
   :cache-release :cache-max-size))

(defpkg :obj/store
  (:documentation "A generic object database protocol based on the STORED metaobject protocol.")
  (:nicknames :store)
  (:import-from :stored :%cache-style)
  (:use :cl :std :stored :sb-mop :btree :id :db :schema :config :cache :val :tensor :cache :io)
  (:export
   #:store
   #:next-oid
   #:next-cid
   #:spec
   #:sset
   #:set-insert
   #:set-list
   #:set-remove
   #:make-sset
   #:stored-object-schema
   #:get-from-root
   #:add-to-root
   #:open-store
   #:close-store
   #:optimize-layout
   #:oid-to-schema-id
   #:default-class-id
   #:default-class-id-type
   #:reserved-oid-p
   #:add-class-store-schema
   #:dropped-instance-p
   #:drop-instance-slots
   #:drop-instance
   #:map-class
   #:map-class-index
   #:store-recreate-instance
   #:recreate-instance
   #:recreate-instance-using-class
   #:get-instances-by-class
   #:get-instances-by-value
   #:valid-stored-reference-p
   #:cross-store-error
   #:signal-cross-store-error
   #:with-store
   #:defstore
   #:serialize-object
   #:deserialize-object))

(eval-always (defvar *obj-packages* *component-packages*))
