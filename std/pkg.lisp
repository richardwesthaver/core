;;; std/pkg.lisp --- Standard Packages

;;

;;; Code:
(in-package :std/defpkg)
;; #+std (in-package :std-int)

(defpkg :std/sym
  (:use :cl)
  (:mix :sb-int)
  (:shadowing-import-from :sb-int :once-only)
  (:shadow :make-gensym :make-gensyms :make-gensym-list)
  (:export
   :ensure-symbol
   :symb
   :format-symbol
   :make-keyword
   :make-slot-name
   :make-gensym
   :make-gensym-list
   :with-gensyms
   :with-unique-names
   :symbolicate
   :keywordicate
   :gensymify
   :gensymify* 
   :fboundp! :vboundp!
   :alias-macro
   :alias-function
   :do-symbols*))

(defpkg :std/list
  (:use :cl)
  (:shadowing-import-from :sb-impl
   :ensure-list :recons :memq :assq
   :proper-list-of-length-p :proper-list-p :singleton-p)
  (:import-from :std/sym :with-gensyms)
  (:import-from :sb-int :doplist)
  (:import-from :sb-kernel :topological-sort)
  (:shadow :group)
  (:export
   :remove-from-plist
   :flatten*
   :safe-endp
   :singleton-p
   :circular-list-error
   :proper-list-length
   :lastcar
   :doplist
   :assoc-value
   :ensure-car
   :ensure-cons
   :appendf
   :nconcf
   :unionf
   :nunionf
   :reversef
   :nreversef
   :removef
   :deletef
   :flatten
   :group
   :firstn
   :zip :unzip
   :zip-list :zip-tree
   :ziptree
   :zipsym
   :ziprm
   :pairs :pair
   :nconsc
   :cart :mapcart
   :cart-case :cart-ecase
   :cart-typecase :cart-etypecase
   :recursive-append :list-dimensions
   :maptree :maptree-if
   :maptree-eki :let-binding-transform
   :ensure-list :recons :memq :assq
   :circular-list :circular-list-p :circular-tree-p :merge!
   :sort!
   :set-equal
   :dcons :dpush
   :dpop :dlist :drdc :dcdr :dcar :dappendf
   :with-dlist :within-dlist
   :topological-sort :match-lambda-lists
   :toposort
   :reconsify :deconsify
   :with-stack-list :with-stack-list*))

(defpkg :std/prim
  (:use :cl :std/list)
  (:import-from :std/sym :symb :with-gensyms)
  (:import-from :sb-introspect :function-lambda-list)
  (:import-from :sb-int :parse-lambda-list)
  (:export 
   :function-lambda-list
   :pswap
   :read-until-end
   :read-lisp-until-end
   :read-lisp-string
   :read-lisp-file
   :*standard-readtable*
   :with-safe-io-syntax
   :call-with-safe-io-syntax
   :safe-read-from-string
   :kernel-class
   :defkernel
   :kernel-object
   :*kernel*
   :kernel
   :kernelp
   :check-kernel
   :make-kernel
   :kernel-expression
   :kernel-info
   :kernel-documentation
   :kernel-init-error
   :no-kernel-error
   :g!-symbol-p
   :defmacro/g!
   :o!-symbol-p
   :o!-symbol-to-g!-symbol
   :defmacro!
   :unquote-args
   :defun!
   :definline
   :defnotinline
   :with-optimization
   :macrofy
   :with-marking
   :using-gensyms
   :binding-gensyms
   :hook
   :value-hook
   :key-hook
   :defhook
   :hook-value
   :add-hook
   :remove-hook
   :concat
   :parse-body
   :destructure-case
   :ensure-function
   :ensure-functionf))

(defpkg :std/condition
  (:use :cl)
  (:shadowing-import-from :asdf :error-name)
  (:import-from :std/list :flatten :removef)
  (:import-from :sb-int :simple-style-warning)
  (:import-from :sb-impl :define-error-wrapper)
  (:shadowing-import-from :sb-ext :retry)
  (:export
   :define-error-wrapper
   :interact
   :interact*
   :interact-line
   :*error-message*
   :*handlers*
   :*verbose*
   :*interactive*
   :*uninteresting-conditions*
   :std-error :error-message
   :define-error-reporter
   :transfer-error
   :invoke-transfer-error
   :transfer-error-report
   :missing-entry
   :entry-replacement-attempt
   :replace-entry-p
   :with-if-failed-handler
   :deferror
   :defcondition
   :car-eql
   :nyi!
   :get-backtrace
   :required-argument
   :out-of-bounds-error
   :ignore-some-conditions
   :simple-style-warning
   :simple-reader-error
   :simple-parse-error
   :simple-program-error
   :circular-dependencies
   :unknown-argument
   :error-name
   :error-kind
   :missing-argument
   :error-item
   :error-items
   :error-reason
   :invalid-argument
   :invalid-item
   :unwind-protect-case
   :protect-abort
   :def-simple-error-reporter
   :std-warning
   :defwarning
   :def-simple-warning-reporter
   :def-warning-reporter
   :meta-condition
   :missing-method
   :missing-methods
   :conflicting-arguments
   :unknown-token
   :condition-handler
   :wrapped-condition
   :wrapped-condition-value
   :wrap-condition
   :wrapped-error
   :wrap-error
   :with-retry-restart
   ;; restarts
   :retry))

(defpkg :std/named-readtables
  (:use :cl :std/prim :std/condition :std/list)
  (:import-from :std/prim :parse-lambda-list)
  (:export
   :defreadtable
   :with-readtable
   :in-readtable
   :make-readtable
   :merge-readtables-into
   :find-readtable
   :ensure-readtable
   :rename-readtable
   :readtable-name
   :register-readtable
   :unregister-readtable
   :copy-named-readtable
   :list-all-readtables
   ;; Types
   :named-readtable-designator
   ;; Conditions
   :readtable-error
   :reader-macro-conflict
   :readtable-does-already-exist
   :readtable-does-not-exist))

(defpkg :std/comp
  (:use :cl)
  (:import-from :std/prim :definline :with-safe-io-syntax :read-lisp-file)
  (:import-from :sb-c :deftransform :defoptimizer 
   :define-vop :parse-deftransform 
   :ctypecase :ctype-array-dimensions :ctypep :define-source-transform
   :inline-vop :immediate-constant-sc :boxed-immediate-sc-p :emit
   :assemble :without-scheduling :inst :inst* 
   :*emit-cfasl* :describe-component :describe-ir2-component
   :make-file-source-info :make-lisp-source-info
   :def-ir1-translator :defknown :ctype-of :type-specifier)
  (:import-from :sb-c :vop)
  (:import-from :sb-c :*compilation-unit* :*backend-sc-numbers* 
   :*backend-sbs* :*backend-sc-names* 
   :*compile-progress* :*compile-component-hook*
   :primitive-object-size :find-saetp :find-saetp-by-ctype)
  (:import-from :sb-vm :*register-arg-tns* :*primitive-objects*
   :primitive-object-name :primitive-object-lowtag :primitive-object-widetag)
  (:import-from :sb-ext :*compiler-print-variable-alist*)
  (:import-from :sb-x86-64-asm :ea :machine-ea)
  (:import-from :sb-assem :dump-symbolic-asm)
  (:export :deftransform :*compiler-print-variable-alist* :parse-deftransform
   :defoptimizer :defknown :ctypecase :ctypep :ctype-array-dimensions :def-ir1-translator
   :*register-arg-tns* :immediate-constant-sc :boxed-immediate-sc-p :*backend-sc-numbers* 
   :*primitive-objects* :*compilation-unit* :define-vop :define-source-transform :inline-vop :vop*
   :*backend-sbs* :*backend-sc-names* :emit :assemble
   :without-scheduling :inst :inst*
   :primitive-object-name :primitive-object-lowtag :primitive-object-widetag :machine-ea
   :*compile-progress* :*emit-cfasl* :*compile-component-hook*
   :describe-component :describe-ir2-component :make-file-source-info :make-lisp-source-info
   :vop :primitive-type-name-of :ctype-of :type-specifier
   :primitive-object-size :find-saetp :find-saetp-by-ctype :deep-size 
   :get-simple-fun-instruction-model :asm :print-form-and-optimize :print-signaled-conditions
   :print-arguments :ea
   :with-ds-lambda-list-parts
   :without-compiler-notes
   :checked-compile :runtime :asm-search :inspect-ir
   :compile-condition :compile-condition-context-format
   :compile-condition-context-arguments :compile-condition-description
   :compile-file-error :compile-warned-warning
   :compile-warned-error :compile-failed-warning
   :compile-failed-error :*compile-file-failure-action*
   :*compile-file-warning-action* :check-lisp-compile-warnings
   :check-lisp-compile-results :reset-deferred-warnings
   :save-deferred-warnings :check-deferred-warnings
   :call-with-saved-deferred-warnings :with-saved-deferred-warnings
   :checked-compile-file)
  (:recycle :sb-c))

(defpkg :std/type
  (:use :cl)
  (:import-from :std/sym :format-symbol :with-gensyms)
  (:import-from :std/list :ensure-car)
  (:import-from :std/prim :definline)
  (:import-from :sb-impl :sfunction)
  (:import-from :sb-c :parse-optional-arg-spec :parse-key-arg-spec :ds-lambda-list-matcher :parse-ds-lambda-list
   :meta-abstractify-ds-lambda-list :ds-lambda-list-match-p)
  (:import-from :std/comp :*primitive-objects* :primitive-object-size 
   :primitive-object-name :primitive-object-lowtag :primitive-object-widetag)
  (:import-from :sb-c :integer-type-length :ctype-of :ctype :widetag-of :lowtag-of)
  (:import-from :sb-kernel :*type-classes* :type-class 
   :*type-cache-nonce* :type-class-name :type-class-id :classoid
   :type-id->type-class :type-hash-value :*ctype-hashsets* :find-classoid 
   :classoid-of :ctype-of :ctype :layout-of 
   :function-designator)
  (:shadowing-import-from :sb-ext :word)
  (:export :+default-element-type+ :function-designator :type-class-of 
   :type-class-id :find-classoid :classoid #:sfunction
   :type-class-id-of :classoid-of :layout-of :type-id<= 
   :type-id< :type-id= :array-type= :type-id->type-class 
   :type-hash-value :type-class-name-of :type-class-name :*type-cache-nonce*
   :*type-classes* :type-class
   :array-index :array-length
   :parse-optional-arg-spec :parse-key-arg-spec 
   :ds-lambda-list-matcher :parse-ds-lambda-list
   :meta-abstractify-ds-lambda-list :ds-lambda-list-match-p
   :parse-meta-ds-lambda-list
   #:negative-double-float :*ctype-hashsets*
   #:abstract-ds-lambda-list
   #:negative-fixnum-p
   #:negative-float
   #:negative-float-p
   #:negative-long-float
   #:negative-long-float-p
   #:negative-rational
   #:negative-rational-p
   #:negative-real
   #:negative-single-float-p
   #:non-negative-double-float
   #:non-negative-double-float-p
   #:non-negative-fixnum
   #:non-negative-fixnum-p
   #:non-negative-float
   #:non-negative-float-p
   #:non-negative-integer-p
   #:non-negative-long-float
   #:non-negative-rational
   #:non-negative-real-p
   #:non-negative-short-float-p
   #:non-negative-single-float
   #:non-negative-single-float-p
   #:non-positive-double-float
   #:non-positive-double-float-p
   #:non-positive-fixnum
   #:non-positive-fixnum-p
   #:non-positive-float
   #:non-positive-float-p
   #:non-positive-integer
   #:non-positive-rational
   #:non-positive-real
   #:non-positive-real-p
   #:non-positive-short-float
   #:non-positive-short-float-p
   #:non-positive-single-float-p
   #:positive-double-float
   #:positive-double-float-p
   #:positive-fixnum
   #:positive-fixnum-p
   #:positive-float
   #:positive-float-p
   #:positive-integer
   #:positive-rational
   #:positive-real
   #:positive-real-p
   #:positive-short-float
   #:positive-short-float-p
   #:positive-single-float
   #:positive-single-float-p
   #:negative-integer
   #:negative-double-float-p
   #:negative-fixnum
   #:negative-integer
   #:negative-integer-p
   #:negative-real-p
   #:negative-short-float
   #:negative-short-float-p
   #:negative-single-float
   #:non-negative-integer
   #:non-negative-long-float-p
   #:non-negative-rational-p
   #:non-negative-real
   #:non-negative-short-float
   #:non-positive-integer-p
   #:non-positive-long-float
   #:non-positive-long-float-p
   #:non-positive-rational-p
   #:non-positive-single-float
   #:integer-type-length
   #:coercef
   #:octet
   #:octet-vector
   #:simple-octet-vector
   #:octet-vector-p
   #:positive-integer-p
   #:positive-long-float
   #:positive-long-float-p
   #:positive-rational-p
   #:of-type
   #:type=
   #:word
   :u1 :u2 :u3 :u4 :u5 :u6 :u7
   :s1 :s2 :s3 :s4 :s5 :s6 :s7 :s8 :s16 :s24 :s32 :s64
   :*simple-types* :*primitive-object-table* 
   :*simple-type-table* :*core-types*
   :*core-type-table* :register-type-id
   :reset-core-types :prim-type 
   :type-id :simple-type-id))

(defpkg :std/string
  (:use :cl :std/sym :std/list :sb-ext)
  (:use-reexport :sb-unicode)
  (:import-from :std/prim :parse-body)
  (:import-from :sb-impl :ef-octets-to-string-fun :ef-string-to-octets-fun)
  (:import-from :sb-kernel :character-coding-error :character-encoding-error :character-decoding-error)
  (:export
   :+cr+ :+lf+
   :+crlf+
   :ef-octets-to-string-fun :ef-string-to-octets-fun
   :character-coding-error :character-encoding-error
   :character-decoding-error :*suppress-character-coding-errors*
   :string-to-octets :octets-to-string
   :*omit-nulls*
   :*whitespaces*
   :*tab-width*
   :string-designator
   :ssplit
   :remove-string
   :trim
   :collapse-whitespaces
   :make-template-parser
   :string-case
   :detabify
   :make-growable-string
   :parse-simple-semver
   :nconcat
   :nconcatf
   :char-range
   :ascii-ichar=
   :ascii-istring=
   :utf8-to-string
   :word-delimiter-p
   :at-delimiter-p
   :*word-delimiters*
   :split-whitespace))

(defpkg :std/num
  (:use :cl)
  (:import-from :sb-int :power-of-two-ceiling)
  (:import-from :std/string :*whitespaces*)
  (:export
   ;; num/parse
   :parse-number
   :parse-real-number
   :parse-positive-real-number
   :invalid-number
   :invalid-number-value
   :invalid-number-reason
   :ensure-integer
   :ensure-number
   ;; num/float
   :make-float-converters
   :encode-float32
   :decode-float32
   :encode-float64
   :decode-float64
   ;; num/leb128
   :read-leb128
   :encode-leb128
   :decode-leb128
   :read-uleb128
   :encode-uleb128
   :decode-uleb128
   ;; num/math
   :power-of-two-ceiling
   :power-of-two-p
   :clamp
   :gaussian-random
   :iota
   :map-iota
   :%lerp
   :%mean
   :%median
   :variance
   :standard-deviation
   :maxf 
   :minf
   :factorial
   :binomial-coefficient
   :subfactorial
   :count-permutations))

(defpkg :std/stream
  (:use :cl :sb-gray)
  (:import-from :std/type :non-negative-integer :positive-integer)
  (:import-from :std/sym :with-gensyms)
  (:import-from :std/prim :definline)
  (:export
   :stream-fd
   :copy-stream
   :wrapped-stream :wrapped-stream-p
   :wrapped-character-input-stream
   :wrapped-character-output-stream
   :counting-character-input-stream
   :prefixed-character-output-stream
   :timestamped-stream
   :mumble-stream :fmt-stream
   :stream-of :char-count-of :line-count-of :col-count-of
   :prev-col-count-of :col-index-of :write-prefix
   :prefix-of
   :with-input-from-file :with-output-to-file))

(defpkg :std/hash
  (:use :cl)
  (:nicknames :std/ht)
  (:recycle :sb-int)
  (:import-from :sb-int 
   :ensure-gethash :map-hashset 
   :hashset-find :hashset-remove
   :hashset-insert :hashset-count
   :psxhash :make-hashset)
  (:import-from :std/prim :definline)
  (:shadowing-import-from :sb-lockless :endp)
  (:import-from :sb-lockless
   :make-so-map/fixnum :+hash-nbits+
   :node-hash :%node-next
   :get-next :node-hash
   :so-head :so-bins
   :so-key :so-data
   :so-count :so-key-node-p
   :so-insert :so-delete
   :so-find :so-find/string
   :so-maplist :make-so-map/string
   :make-so-set/string :make-so-set/fixnum :make-so-map/addr :make-marked-ref
   :make-so-set/addr :unbound-marker-p)
  (:export :hash-table-alist
   :hash-table-list :copy-hash
   :maphash-keys :hash-table-keys
   :maphash-values :hash-table-values
   :alist-hash-table :plist-hash-table 
   :plist-string-hash-table :make-hashset
   :hash-table-plist :ensure-gethash
   :pophash :*global-hasher*
   :*global-hash* :djb
   :hash-object :hash-object-address
   :dumb-string-hash :table
   :map-hashset :hashset-find :hashset-remove :hashset-insert 
   :hashset-count :psxhash))

(defpkg :std/curry
  (:use :cl :std/prim)
  (:import-from :std/list :mappend)
  (:import-from :std/sym :make-gensym-list)
  (:export
   :disjoin
   :conjoin
   :compose
   :multiple-value-compose
   :curry
   :rcurry
   :map-product
   :rec))

(defpkg :std/readtable
  (:use :cl :std/prim)
  (:import-from :std/named-readtables :defreadtable :in-readtable)
  (:import-from :std/curry :curry :rcurry :compose)
  (:import-from :std/sym :symb)
  (:import-from :std/prim :defmacro!)
  (:export
   :ignore-numarg
   ;; readtable
   :|#"-reader|
   :|#`-reader|
   :|#f-reader|
   :|#$-reader|
   :|[-reader|
   :|{-reader|
   :|#l-reader|
   :segment-reader
   :match-mode-ppcre-lambda-form
   :subst-mode-ppcre-lambda-form
   :|#~-reader|
   :_))

(defpkg :std/macs
  (:use :cl :std/prim)
  (:import-from :std/sym :symb :mkstr :make-gensym-list :with-gensyms :symbolicate :keywordicate)
  (:import-from :sb-int :make-macro-lambda :parse-lambda-list :lambda-list-keyword-mask :check-lambda-list-names)
  (:import-from :std/curry :compose)
  (:import-from :std/named-readtables :in-readtable)
  (:import-from :std/list :flatten :recursive-append :zip-tree :group :let-binding-transform :remove-from-plist)
  (:import-from :std/prim :defmacro! :defun! :defmacro/g! :g!-symbol-p :o1-symbol-to-g!-symbol :parse-body)
  (:import-from :sb-loop :*loop-ansi-universe* :loop-standard-expansion)
  (:export
   :*loop-ansi-universe*
   :loop-standard-expansion
   :this :self
   :make-macro-lambda
   :parse-lambda-list
   :lambda-list-keyword-mask
   :check-lambda-list-names
   :once-only
   :define-class
   :defclass*
   :dlet
   :named-lambda
   :nested-loop
   :dlambda
   :until
   :build-batcher-sn
   :sortf
   :dollar-symbol-p
   :if-match
   :when-match
   :destructuring-case
   :destructuring-ccase
   :destructuring-ecase
   :when-let
   :when-let*
   :if-let
   :if-let*
   :if*
   :define-constant
   :define-constant*
   :defvar-unbound
   :def!
   :defonce
   :eval-always
   :eval-every
   :compile-and-eval
   :compile-and-eval*
   :compile-and-load
   ;; ana
   :awhen
   :acond
   :acase
   :atypecase
   :alambda
   :nlet-tail
   :alet*
   :alet
   :acond2
   :aif
   :it
   :%a
   ;; pan
   :%p
   :pandoriclet
   :pandoriclet-get
   :pandoriclet-set
   :pandoric-get
   :get-pandoric
   :with-pandoric
   :pandoric-hotpatch
   :pandoric-recode
   :plambda
   :defpan
   :pandoric-eval
   :with-collectors
   :collecting
   :collect
   :switch
   :eswitch
   :cswitch
   :xor
   :ifret
   :deftyped
   :deftyped*
   :defityped
   :defityped*
   :letv*
   :lety
   :lety*
   :defunits :unit-of-distance 
   :distance-designator
   :defwith
   :with-memoization
   :memoizing
   :match :lambda-match
   :multiple-value-case))

(defpkg :std/array
  (:use :cl)
  (:import-from :sb-ext :maybe-inline)
  (:import-from :std/prim :definline)
  (:import-from :std/list :maptree-eki :zip)
  (:import-from :sb-kernel :with-array-data :array-rank-limit)
  (:import-from :std/macs :eval-every :lety* :destructuring-case)
  (:import-from :sb-c :check-bound)
  (:export :copy-array :signed-array-length :array-shift 
   :check-bound :vector-push-extend-position :vector-pop-position :vector-copy
   :vectorify :make-array-allocator
   :vector-foldl :vector-foldr
   :vector-map-foldl :vector-map-foldr
   :vector-max :vector-min
   :vector-eq :with-array-data
   :vector-to-list :copy-vector-to-list
   :modproj :simplify-array :array-rank-limit :sort-index 
   :binary-search :element-type))

(defpkg :std/sys
  (:use :cl :sb-int)
  (:import-from :std/condition :*interactive*)
  (:import-from :sb-kernel :get-lisp-obj-address :with-pinned-objects 
   :unbound-marker-p :generation-of
   :current-sp :current-fp 
   :make-unbound-marker)
  (:import-from :std/prim :definline :defhook)
  (:import-from :sb-impl :*external-formats*)
  (:import-from :sb-vm :list-allocated-objects :fun-signature=
   :map-allocated-objects :fset :*linkage-name-map* :ldb-monitor
   :map-immobile-objects :memory-usage :references-p :show-ctype-ctor-cache-metrics
   :n-lowtag-bits :lowtag-mask :lowtag-limit :n-fixnum-tag-bits
   :fixnum-tag-mask :n-fixnum-bits :word-shift :n-word-bytes
   :n-machine-word-bytes :n-widetag-bits :widetag-mask :most-positive-word
   :lowtag-of :widetag-of :hexdump :print-allocated-objects
   :c-find-heap->arena :*arena-exhaustion-handler*
   :arena :arena-p
   :arena-bytes-used :arena-bytes-wasted
   :arena-length :arena-userdata
   :new-arena :destroy-arena
   :hide-arena :unhide-arena
   :switch-to-arena :rewind-arena
   :unuse-arena :in-same-arena
   :dump-arena-objects :arena-contents
   :points-to-arena :show-heap->arena)
  (:import-from :sb-sys :int-sap :find-foreign-symbol-address)
  (:import-from :sb-fasl :*assembler-routines* :+fasl-file-version+ 
   :*fasl-file-type* :get-asm-routine :asm-routine-index-from-addr :check-fasl-header 
   :*show-fops-p* :load-as-fasl)
  (:use-reexport :sb-cltl2)
  (:recycle :sb-assem)
  (:recycle :sb-ext)
  (:import-from :sb-c :lexenv-user-data :lexenv-find 
   :make-null-lexenv :name-reserved-by-ansi-p :default-gc-strategy :open-fasl-output 
   :close-fasl-output :fasl-output)
  (:import-from :sb-c :parse-eval-when-situations :source-location :*backend-byte-order* :*backend-primitive-type-names* :*backend-primitive-type-aliases* :*backend-predicate-types* :*backend-type-predicates* :primitive-type-name :primitive-type)
  (:recycle :sb-sys)
  (:import-from :sb-ext :maybe-inline :defglobal :define-load-time-global :finalize :cancel-finalization)
  (:import-from :std/sym :with-gensyms :search-roots :vboundp!)
  (:import-from :std/list :appendf)
  (:import-from :sb-assem :*backend-instruction-set-package*)
  (:import-from :sb-impl :*logical-hosts* :make-logical-host 
   :logical-host :info :show-info :*info-types*
   :*finalizer-thread* :show-finalizers :dx-flet :dx-let
   :read-only-space-obj-p :dynamic-space-obj-p :tune-image-for-dump :get-external-format)
  (:import-from :sb-debug :untrace-all :untrace-package)
  (:import-from :sb-ext :fold-identical-code)
  (:import-from :sb-vm :primitive-type-of)
  (:import-from :std/macs :if-let :defmacro! :eval-always)
  (:export
   :*backend-primitive-type-names* 
   :*backend-primitive-type-aliases* 
   :*backend-predicate-types* 
   :*backend-type-predicates* 
   :primitive-type-name 
   :primitive-type 
   :primitive-type-of
   :lisp-implementation-id
   :lisp-machine-id
   :machine-target
   :+lowtags+ :+widetags+
   :open-fasl-output :close-fasl-output
   :check-fasl-header
   :check-fasl-file-header
   :load-as-fasl
   :featurep
   :*show-fops-p*
   :make-unbound-marker
   :*external-formats*
   :get-external-format
   :int-sap :print-allocated-objects
   :current-sp :current-fp
   :hexdump :hexdump-object
   :n-lowtag-bits :lowtag-mask :lowtag-limit :n-fixnum-tag-bits
   :fixnum-tag-mask :n-fixnum-bits :word-shift :n-word-bytes
   :n-machine-word-bytes :n-widetag-bits :widetag-mask
   :most-positive-word
   :tune-image-for-dump
   :show-ctype-ctor-cache-metrics
   :memory-usage
   :references-p
   :untrace-all
   :defprinter
   :untrace-package
   :ldb-monitor
   :read-only-space-obj-p
   :dynamic-space-obj-p
   :asm-routine-index-from-addr
   :*assembler-routines*
   :+fasl-file-version+
   :*fasl-file-type*
   :get-asm-routine
   :fun-signature=
   :fset
   :*linkage-name-map*
   :map-immobile-objects
   :map-allocated-objects
   :fold-identical-code
   :*finalizer-thread*
   :show-finalizers
   :with-pinned-objects
   :finalize
   :cancel-finalization
   :get-lisp-obj-address
   :list-allocated-objects
   :generation-of
   :default-gc-strategy
   :name-reserved-by-ansi-p
   :*backend-byte-order*
   :.i ;; alias for *inspected*
   :info
   :maybe-inline
   :defglobal :define-load-time-global
   :parse-eval-when-situations 
   :source-location
   :lexenv-user-data
   :lexenv-find
   :make-null-lexenv
   :64-bit-p :32-bit-p
   :*logical-hosts*
   :save-shared-objects
   :make-logical-host
   :logical-pathname-translation
   :logical-host :info 
   :show-info :*info-types*
   :sbcl-hooks
   :*default-arena-size*
   :current-lisp-implementation
   :current-machine
   :*machine-target*
   :*machine-targets*
   :machine-target
   :list-package-symbols
   :list-all-symbols
   :do-internal-symbols
   :package-symbols
   :package-symbol-names
   :define-logical-pathname
   :list-all-logical-host-names
   :check-logical-host
   :check-logical-hosts
   :save-lisp-tree-shake-and-die
   :forget-shared-object
   :forget-shared-objects
   :save-lisp
   :without-fp-traps
   :little-endian-p
   :cpuid
   :cpu-vendor
   :cpu-brand
   :get-real-time-seconds 
   :time-remaining 
   :with-countdown
   ;; arenas
   :c-find-heap->arena :*arena-exhaustion-handler*
   :arena :arena-p
   :arena-bytes-used :arena-bytes-wasted
   :arena-length :arena-userdata
   :new-arena :destroy-arena
   :hide-arena :unhide-arena
   :switch-to-arena :rewind-arena
   :unuse-arena :in-same-arena
   :arena-contents
   :points-to-arena
   :show-heap->arena
   :lisp-object-info
   ;; system paths
   :*stash*
   :find-stash-directory
   :stash-pathname
   :find-store-directory
   :store-pathname))

(defpkg :std/bit
  (:use :cl :std/macs)
  (:import-from :std/type :octet :octet-vector)
  (:mix :sb-sys)
  (:export
   :+hex-digits+
   :read-n-bytes
   :make-bits
   :sign-bit
   :different-signs-p
   :mortify-bits
   :int-list-bits
   :aref-bit
   :make-bit-vector
   :signed-to-unsigned
   :unsigned-to-signed
   :logbit
   :bitfield
   :bitfield-slot-name
   :bitfield-slot-start
   :bitfield-slot-end
   :bitfield-slot-size
   :bitfield-slot-initform
   :bitfield-slot-pack
   :bitfield-slot-unpack
   :parse-atomic-bitfield-slot-specifier
   :parse-compound-bitfield-slot-specifier
   :bitfield-slot
   :bitfield-boolean-slot
   :bitfield-integer-slot
   :bitfield-member-slot
   :define-bitfield
   :hex-string-to-octet-vector
   :hex-string
   :octet-vector-to-hex-string
   :octets-to-integer
   :integer-to-octets
   :octets-to-integer-le
   :integer-to-octets-le
   :read-little-endian
   :write-little-endian
   :hexchar-to-int
   :make-octets
   :octets
   :flags-case))

(defpkg :std/io
  (:use :cl :std/sys :std/type)
  (:import-from :std/prim :definline :parse-body)
  (:import-from :std/condition :deferror)
  (:import-from :std/macs :when-let :eval-always :once-only)
  (:import-from :std/sym :symbolicate :with-gensyms)
  (:import-from :std/type :octet-vector :*type-classes* 
   :type-class-name-of :type-class-name :type= :u8 :u16 :u24 :u32 :u64)
  (:export :define-io
   :u8 :u16 :u24 :u32 :u64 :align
   :serde :ser :de :serialize 
   :*stream-read-positions* :stream-read-position
   :deserialize :serde-condition :serde-error :serializer-error 
   :deserializer-error :*io-table*
   :with-binary-readers :with-binary-writers))

(defpkg :std/alien
  (:use :cl :sb-alien)
  (:import-from :std/sym :symbolicate :keywordicate :with-gensyms)
  (:import-from :std/array :element-type)
  (:import-from :std/sys :little-endian-p :32-bit-p)
  (:import-from :std/condition :out-of-bounds-error :invalid-item)
  (:import-from :std/bit :make-octets)
  (:import-from :std/macs :with-memoization :memoizing :destructuring-case :once-only :compile-and-eval)
  (:import-from :std/type :octet-vector :octet :array-index)
  (:import-from :sb-int :with-float-traps-masked)
  (:import-from :std/io :define-io)
  (:import-from :sb-posix :lisp-for-c-symbol :define-designator :*designator-types* :filename)
  (:import-from :sb-alien :%alien-value :sap+ 
   :*linkage-info* :*shared-objects* :*alien-type-classes* :alien-type-class 
   :unparse-alien-type :parse-alien-type :pick-lisp-and-alien-names :alien-type-bits
   :alien-type-p :alien-value-p :alien-callback-p :alien-void-type-p
   :alien-pointer-type-p :int-sap :alien-value :alien-value-type 
   :alien-typep :alien-size
   ;; alien types
   :c-string :int :enum :short
   :long :slot :void :cast :char :addr
   :alien :deref :double :union
   :signed :unsigned :long :long-long
   :sap-alien :alien-sap :make-alien :free-alien
   :make-alien-string :naturalize :deport :deport-alloc
   :%alien-value :alien-funcall :define-alien-variable :alien-value
   :extern-alien :with-alien :compute-lisp-rep-type :compute-alien-rep-type)
  (:import-from :sb-sys :system-area-pointer :vector-sap :with-pinned-objects :sap-int)
  (:import-from :sb-ext :array-storage-vector)
  (:export
   :alien-array
   :c-string :int 
   :enum :long :slot :void
   :cast :char :addr :short
   :null-pointer :null-pointer-p
   :alien :deref
   :double :union
   :signed :unsigned
   :long :long-long
   :system-area-pointer
   :parse-alien-type
   :unparse-alien-type
   :naturalize :deport :deport-alloc :%alien-value
   :compute-lisp-rep-type :compute-alien-rep-type
   :alien-value :lisp-for-c-symbol
   :*alien-load-table* :load-alien
   :load-aliens
   :define-alien-variable :setfa
   :alien-size :alien-size*
   :*alien-type-classes* :*linkage-info*
   :*shared-objects* :with-vector-sap
   :sap-alien :alien-sap
   :make-alien :free-alien
   :alien-funcall :make-alien-string
   :double-array-pointer :float-array-pointer
   :octet-vector-pointer :copy-c-string
   :clone-strings :clone-octet-vector-list
   :clone-integer-list :clone-octet-vector-list*
   :octets-to-alien-array :foreign-type-size
   :with-foreign-pointer :with-foreign-object
   :with-foreign-objects :with-alien-slots
   :with-alien :extern-alien
   :clone-octets-to-alien :octets-to-alien
   :clone-octets-from-alien :foreign-int-to-integer
   :foreign-int-to-bool :bool-to-foreign-int
   :alien-enum-keys :alien-enum-values
   :define-alien-enum :define-alien-routine
   :define-opaque :shared-object-name
   :define-alien-loader :c-strings-to-string-list
   :read-alien-signed-byte-32 :read-alien-fixnum
   :read-alien-signed-byte-64 :read-alien-unsigned-byte-32
   :read-alien-unsigned-byte-64 :read-alien-single-float
   :read-alien-double-float :write-alien-signed-byte-32
   :write-alien-fixnum :write-alien-unsigned-byte-32
   :write-alien-unsigned-byte-64 :write-alien-single-float
   :write-alien-double-float :offset-char-pointer
   :write-alien-signed-byte-64 :*cpus*
   :num-cpus :list-all-shared-objects
   :alien-or-lisp-octets :foreign-alloc
   :foreign-free :read-alien
   :write-alien :loff-t
   :pid-t :uid-t
   :gid-t :memset
   :memcpy :posix-memalign
   :timeval :timespec
   :sap :sap-int
   :alloc :free
   :push-sap :push-sap*
   :pull-sap :pull-sap*
   :alien-to-element-type :element-type-to-alien
   :defar :sap-svref
   :sap-ref :sap-set
   :foreign-vector :foreign-vector-class
   :foreign-vector-length :foreign-vector-element-type
   :fvref :bref
   :iobuf :bzero
   :defsyscall :*syscall-type-table*
   :syscall-return-type))

(defpkg :std/meta
  (:use :cl :sb-pcl)
  (:use-reexport :sb-mop)
  (:import-from :std/sym :symb :make-keyword :with-gensyms)
  (:import-from :std/named-readtables :readtable :readtable-name)
  (:import-from :std/list :toposort)
  (:import-from :std/hash :make-hashset :hashset-find :hashset-insert :psxhash)
  (:import-from :sb-ext :without-package-locks)
  (:import-from :std/macs :eval-always :if-let :when-let)
  (:import-from :std/prim :definline)
  (:shadow :reset)
  (:export :list-slot-values-using-class
   :defverb :*verbs*
   :list-class-methods :list-class-slots :ensure-finalized :subclassp :write-object :start
   :stop :stopped-p :shutdown :reset
   :defaccessor :defaccessor* :defmethods :defclass!
   :data :name :tags :shallow-copy-object
   :exec :copy-object :safe-superclasses :run-object
   :slot-boundp* :slot-values
   :explore :with-fslots
   :upgrade :version
   :validate :resume
   :deadline :sync
   :lock :bind
   :head :tail
   :call :swap
   :copy :assign
   :clean :purge
   :assignee :started-p
   :verb-p :init
   :reset :state
   :init* :pause
   :install :uninstall
   :send :receive
   :build :build-from
   :class-equalp :slots-boundp
   :*standard-metaobjects* :find-slot-def-by-name
   :find-direct-slot-def-by-name :find-slot-defs-by-type
   :find-slot-def-names-by-type :struct-slots-and-values
   :slots-and-values :struct-constructor
   :remove-template-method :define-template-method
   :define-template-generic :*template-table*
   :template-function-p :*sham-classes*
   :defsham :save
   :clone :define-class-map))

(defpkg :std/seq
  (:use :cl :std/prim)
  (:shadow :queue :make-queue :queue-count :queue-empty-p)
  (:import-from :sb-thread :with-mutex :make-mutex :condition-notify :make-waitqueue :condition-wait)
  (:import-from :std/macs :once-only :when-let :defonce :unwind-protect-case :eval-always :letv*)
  (:import-from :std/sym :with-gensyms)
  (:import-from :std/meta :data :defaccessor :lock)
  (:import-from :std/condition :invalid-item)
  (:import-from :std/list :firstn :dcons :dcdr :dcar :dpush :dpop :with-dlist :within-dlist)
  (:import-from :sb-int :collect)
  (:import-from :std/prim :definline)
  (:import-from :std/array :signed-array-length)
  (:import-from :std/sym :symbolicate)
  (:import-from :std/type :array-length :array-index)
  (:import-from :std/sys :get-internal-time-seconds :time-remaining :with-countdown)
  (:export :sequencep :take :starts-with-subseq 
   :take* :starts-with
   :unsplice :item-predicate
   :dosequence :ends-with
   :ends-with-subseq :nth-value-or
   :split-sequence :split-sequence-if :split-sequence-if-not :starts-with-p
   :starts-with-one-of-p :copy-n
   :basic-queue :raw-queue-count :raw-queue :make-raw-queue
   :pop-raw-queue :peek-raw-queue :raw-queue-empty-p :raw-queue-full-p
   :raw-queue-capacity :cons-queue :push-cons-queue :raw-queue
   :pop-cons-queue :make-cons-queue :peek-cons-queue :cons-queue-empty-p
   :push-queue :push-queue* :pop-queue :pop-queue* :peek-queue :peek-queue*
   :queue-count :queue-count* :queue-empty-p :queue-empty-p* :queue-full-p :queue-full-p*
   :try-pop-queue :try-pop-queue* :call-with-queue-lock :with-queue-lock
   :queue :make-queue
   ;; priority-queue
   :priority-queue :vector-queue
   :*default-priority* :*default-priority-queue-size*
   :push-priority-queue :pop-priority-queue
   ;; pqueue
   :pqueue :make-pqueue
   :pqueue-insert :pqueue-remove :pqueue-extract-maximum :pqueue-reorder
   :pqueue-emptyp :pqueue-maximum :pqueue-contents :pqueue-keyfun
   ;; spin queue
   :spin-queue :make-spin-queue :push-spin-queue :make-spin-lock
   :with-spin-lock :pop-spin-queue :peek-spin-queue :spin-queue-count 
   :spin-queue-empty-p :fib-heap
   ;; fib-heap
   :make-heap :fib-insert :extract-min :fib-delete
   ;; accumulator
   :accumulated :accumulate :accumulator :max-accumulator 
   :min-accumulator :counter :make-counter :counter-value 
   :inc-counter :dec-counter
   ;; iterator protocol
   :iter :iterator :next :prev
   :skey :sval
   :seek :seek-to-first
   :seek-to-last :seek-for-prev
   :iter-valid-p :*iter*
   :idx :with-iter
   :make-priority-queue :do-indexes
   :mod-incf :mod-decf
   :repeat :pqueue-empty-p))

(defpkg :std/path
  (:use :cl)
  (:import-from :uiop :directory-files :subdirectories :absolute-pathname-p :pathname-equal
   :pathname-parent-directory-pathname)
  (:export
   :pathname-equal
   :pathname-parent-directory-pathname
   :directory-files
   :subdirectories
   :path
   :directory-wildcard
   :wild-pathname
   :file-pathname
   :non-wild-pathname
   :absolute-pathname
   :relative-pathname
   :directory-pathname
   :directory-empty-p
   :symlink-pathname
   :symlinkp
   :directory-path
   :directory-path-p
   :merge-homedir-pathnames
   :ensure-directory-truename
   :absolute-directory-pathname
   :ensure-absolute-pathname
   :+wildfile+ :+pathsep+ :set-pathname-suffix :*tmp-suffix*
   :*tmp*
   :tmpize-pathname
   :tmp-path
   :with-directory
   :call-with-directory
   :with-tmp
   :walk-directory))

(defpkg :std/file
  (:use :cl)
  (:import-from :std/macs :define-constant :eval-always :once-only :when-let)
  (:import-from :std/condition :deferror)
  (:import-from :std/path :directory-path :directory-path-p)
  (:import-from :std/stream :copy-stream)
  (:import-from :std/type :octet :octet-vector :array-index :array-length :+default-element-type+)
  (:import-from :sb-ext :delete-directory :delete-file-error)
  (:import-from :uiop :delete-file-if-exists)
  (:export
   :delete-directory :delete-file-error
   :unknown-file-type
   :delete-file-if-exists
   :probe-delete-file
   :probe-delete-directory
   :delete-directories
   :read-file
   :file-read-forms
   :tmpfile
   :dir
   :file
   :with-open-files
   :write-stream-into-file
   :write-file-into-stream
   :file=
   :file-size
   :file-size-in-octets
   :octet-vector=
   :file-date
   :file-write-date*
   :file-timestamp
   :*hidden-paths*
   :hidden-path-p
   :find-files
   :count-file-lines
   :probe-merge-file
   :probe-directory
   :move-file))

(defpkg :std/pipe
  (:use :cl :std/array :std/meta)
  (:import-from :std/condition :required-argument :invalid-item :invalid-argument)
  (:import-from :std/sym :with-gensyms)
  (:import-from :std/type :octet)
  (:import-from :std/macs :when-let :eval-always :once-only)
  (:import-from :std/list :removef)
  (:import-from :std/file :file)
  (:export :sink :source :element 
   :pipe :msg :print-filter :switch-filter :predicate-filter :bin :predicate :filter
   :element-stream :value :index :resolve-element
   :find-element :find-parent-element :insert-element :withdraw-element
   :remove-element :set-element-id :move-element :message
   :condition-message :message-condition
   :stream-sink :stream-source :file-sink :file-source
   :add-element :insert-element*
   :defpipe :make-pipe :simple-message :message-content
   :defpipe* :event :bus :format-message))

(defpkg :std/thread
  (:use :cl :std/prim)
  (:shadowing-import-from :std/seq :queue-empty-p :queue :queue-count :make-queue)
  (:use :sb-thread :std/meta :std/macs :std/sym :std/type :std/condition :std/seq)
  (:import-from :std/seq :do-indexes :repeat)
  (:import-from :std/pipe :index :make-pipe :source :sink :filter :event :message)
  (:import-from :sb-thread :*all-threads*)
  (:import-from :std/list :flatten)
  (:import-from :std/prim :definline)
  (:import-from :std/prim :defmacro!)
  (:import-from :std/prim :ensure-function)
  (:import-from :std/macs :eval-always)
  (:use-reexport :sb-thread)
  (:import-from :std/macs :if-let :eval-always)
  (:import-from :std/list :deletef)
  (:export
   :with-timeout* :update-limiter-count
   :limiter-lock :limiter-count
   :exit-thread-pools :*default-spint-count*
   :make-ephemeral-thread :*all-threads*
   :*worker-class* :*worker*
   :+work-tag+ :work
   :scheduler :make-scheduler
   :schedule-work :submit-raw-work
   :worker-kernel-function :pool-kernel-function
   :*worker-kernel* :run-with-timer
   :timer-expired-p
   :*pool-kernel* :*thread-pool*
   :*thread-pool-table* :find-work
   :do-workers :submit-work
   :submit-indexed :with-temp-pool
   :call-with-temp-pool :with-thread-pool
   :with-channel :receive-indexed
   :with-submit-indexed :submit-with-cancel
   :with-submit-counted :submit-counted
   :receive-counted :submit-cancelable
   :receive-cancelables :receive-result
   :try-receive-result :shutdown-channel
   :broadcast-work :find-thread-pool
   :run-thread :thread-support-p
   :print-top-level :println-top-level
   :find-thread-by-id :thread-id-list
   :timed-join-thread :kill-thread
   :wait-for-threads :worker
   :wait-for-worker :make-oracle
   :hang :finish-threads
   :kill-worker :kill-workers
   :kill :join-worker
   :start-worker :start-workers
   :start-workers* :workers*
   :scheduler* :biased-scheduler
   :make-channel :run-worker
   :with-default-special-bindings :worker-thread
   :*worker-restarts* :worker-count
   :worker-count* :worker-index*
   :oracle :oracle-id :find-thread
   :make-threads :with-threads 
   :with-thread :with-temp-pool
   :thread-count :channel
   :channel-pool :channel-queue
   :thread-pool :workers
   :make-thread-pool :end-thread-pool
   :pop-worker :make-worker*
   :make-worker :designate-oracle
   :make-workers :unwrap-result
   :condition-wait* :sync-message
   :with-sync-message :schedule
   :supervisor :supervisor-thread
   :domain :scope
   :+standard-io-bindings+ :*default-special-bindings*
   :check-thread-pool :*oracle-table*
   :*worker-threads* :*super-threads*
   :compute-special-bindings
   :thread-pipe :source-worker
   :sink-worker :filter-worker
   :worker-message :worker-event
   :timer-p))

(defpkg :std/async
  (:use :cl :std/thread :std/prim :std/seq :std/sym :std/list :std/macs)
  (:import-from :std/meta :state)
  (:import-from :std/macs :with-gensyms :when-let)
  (:export :future :promise :await
   :future-kernel :fulfill :fulfilledp :declaim-defpun
   :speculate :defpun :defptyped :plet 
   :plet-if :pcount :pcount-if :pcount-if-not
   :pdotimes :por :pand :pnotany
   :pnotevery :psome :pevery :pmap-reduce
   :pmapcon :pmapcan :pmapc :pmapl
   :pmaplist :pmaplist-into :pmapcar :pmap
   :pmap-into :preduce-partial :preduce :pfind
   :pfind-if :pfind-if-not :*defpuns* ))

(defpkg :std/task
  (:use :cl :std/thread :std/meta :std/seq :std/prim :std/async)
  (:import-from :std/thread :%make-thread)
  (:import-from :std/type :positive-fixnum)
  (:import-from :std/macs :if-let)
  (:export
   :task-schedule
   :jobs
   :tasks
   :results
   :result
   :*task-class*
   :*task-priority*
   :*tasks*
   :*jobs*
   :*stages*
   :*task*
   :*result*
   :task :job 
   :dependencies :dependents
   :async-task
   :scheduled-task
   :make-task
   :make-job
   :run-job
   :job-p :task-p :task :task-worker
   :plan :status :planner :plan-bits
   :task-pool
   :task-done-p :record-dependency
   :simple-task :simple-plan
   :pressure :task-forced-p :task-prevented-p :pressure-parameters
   :mark-task-done))

(defpkg :std/rand
  (:use :cl)
  (:import-from :std/type :octet)
  (:export
   :shuffle
   :random-elt
   :random-ref
   :random-char
   :random-chars
   :random-bytes
   :random-booleans
   :random-do))

(defpkg :std/print
  (:use :cl :std/stream :std/string)
  (:import-from :std/list :group :ensure-cons :assoc-value)
  (:import-from :std/meta :init)
  (:import-from :std/sym :with-gensyms)
  (:import-from :std/rand :random-booleans)
  (:import-from :sb-ext :*print-circle-not-shared* :*suppress-print-errors*)
  (:import-from :sb-impl :prin1-to-line)
  (:shadowing-import-from :uiop :println)
  (:export :printer-status :fmt-row :format-sxhash 
   :fmt-column :*annotations*
   :iprintln :fmt-tree :println :human-readable-size 
   :print-slots :format-slots :*print-slot-indent* :make-bitmap
   :with-bitmap :set-pixel :outside-bounds :draw
   :pattern-to-bitmap :draw-border :circle :bullseye
   :moire :line :sunbeam :fill-bitmap 
   :filled-circle :sun :peace :with-comic-strip
   :plot-function :print-table :print-heading :print-in-box
   :smile :draw-one-in-chance :draw-chance :mumble
   :*mumble-timestamp* :deffmt :defprint :define-printer
   :*printer-table* :find-printer :with-printer :use-printer
   :in-printer :*default-printer* :*print-color* :copy-printer
   :*annotation-table* :with-annotations
   :save-annotations :copy-annotations
   :load-annotations :defnotation
   :aformat :expand-annotated-string
   :expand-annotation :word-wrap
   :annotations))

(defpkg :std/os
  (:use :cl :sb-alien :std/string)
  (:import-from :std/macs :with-gensyms :if-let :when-let :eval-always)
  (:import-from :std/prim :definline)
  (:import-from :std/sys :define-logical-pathname :add-logical-pathname-translation)
  (:import-from :std/file :probe-directory)
  (:import-from :std/path :directory-path :merge-homedir-pathnames)
  (:import-from :std/hash :hash-table-keys)
  (:import-from :sb-posix :tcgetattr :tcsetattr 
   :termios :termios-cc :termios-cflag :termios-iflag 
   :termios-oflag :termios-lflag)
  (:import-from :sb-unix 
   :unix-fast-select :fd-set :fd-clr :fd-isset
   :fd-zero :unix-stat :unix-fstat :unix-lstat
   :unix-file-mode :unix-pid :unix-uid :unix-gid
   :syscall-type :syscall :syscall* :int-syscall
   :type-syscall :void-syscall)
  (:import-from :std/alien :defar)
  (:import-from :sb-impl :find-a-pty :open-pty)
  (:export
   :unix-file-mode :unix-pid :unix-uid :unix-gid
   :unix-fast-select :fd-set :fd-clr :fd-isset
   :fd-zero :unix-stat :unix-fstat :unix-lstat
   :fd-type :syscall-type :syscall :syscall*
   :int-syscall :type-syscall :void-syscall
   :*user-fasl-cache*
   :user-fasl-cache
   :ensure-fasl-cache-file
   :fasl-cache-file
   :resolve-fasl-cache-file
   :find-a-pty :open-pty :sudo-p :forkable-p
   :user-info :user-add
   :group-add :get-host-name
   :list-all-users :list-all-groups
   :with-umask :with-fd
   :cfmakeraw :ioctl
   :termios-iflag :termios-oflag :termios-lflag :termios-cflag
   :pathname-executable-p :set-signal-handler
   :open-pipe
   :+tiocgwinsz+ :+tiocswinsz+ :+tiocnotty+ :+tcsanow+
   :+tcsaflush+ :+tcsadrain+
   :+opost+ :current-user
   :*xdg-dir-table* :xdg-dir
   :init-xdg-dirs :xdg-config-file
   :xdg-config-directory :xdg-config-dir
   :xdg-data-directory :xdg-data-dir
   :xdg-cache-directory :xdg-cache-dir
   :xdg-runtime-directory :xdg-runtime-dir
   :xdg-state-directory :xdg-state-dir
   :termios :winsize
   :isatty :make-symlinks
   :tcgetattr :tcsetattr :tcgetattr* :tcsetattr*
   :relative-pathname-p :absolute-pathname-p
   :unmerge-pathnames :current-directory
   :with-directory-iterator :file-kind
   :merge-env-pathnames :enable-echo
   :disable-echo :without-echo))

(pkg:defpkg :std/defsys
  (:nicknames :defsys)
  (:use :cl :std/prim :std/meta 
    :std/macs :std/thread :std/task :std/io 
    :std/seq :std/pipe :std/prim :std/condition
    :std/print :std/meta :std/path :std/sym
    :std/macs :std/type :std/os :std/defpkg
    :std/list :std/hash)
  (:import-from :std/named-readtables :in-readtable :readtable-name)
  (:import-from :std/comp :checked-compile-file)
  (:import-from :sb-impl :*requiring* :module-provide-contrib)
  (:shadow :load-system :compile-system :find-system :system)
  (:import-from :asdf :module-provide-asdf :defsystem)
  (:shadowing-import-from :std/meta :version)
  (:export 
   :*sysdefs*
   :*asdf-compatibility*
   :*defsys*
   :*system-table*
   :*module-table*
   :*provider-table*
   :components
   :sysdefs
   :sysdef
   :defsys
   :system-path
   :system-home
   :system-relative-pathname
   :list-all-systems
   :list-all-test-systems
   :list-all-providers
   :list-all-modules
   :register-module
   :module-provide
   :module-require
   :defprovider
   :defcomponent
   :load-sys
   :compile-sys
   :system
   :read-component
   :compile-component
   :load-component
   :reload-system-packages
   :component
   :mod-component
   :file-component
   :pkg-component
   :dir-component
   :grovel-component
   :component-type
   :component-package
   :component-require
   :find-system
   :save-system
   :make-system
   :find-module
   :*module*
   :*module-table*
   :module
   :use :using
   :refuse :refusing
   :load-module
   :load-modules
   :unload-module
   :with-module
   :load-system
   :compile-system
   :test-system
   :with-system-session
   :make-system-session
   :*system-session*
   :*system-table*
   :+sys-extension+
   :find-component))

(defpkg :std
  (:use :cl)
  (:use-reexport :std/named-readtables :std/defpkg :std/condition
   :std/sym :std/list :std/type :std/num 
   :std/stream :std/curry :std/array :std/hash
   :std/alien :std/meta :std/thread :std/task
   :std/macs :std/bit :std/print :std/path
   :std/os :std/file :std/string :std/sys 
   :std/readtable :std/pipe :std/io :std/rand 
   :std/async :std/seq :std/prim :std/comp 
   :std/defsys))

(define-lisp-package :std)

(defpkg :std-user
  (:use :std-lisp :sb-ext :sb-alien 
    :sb-thread :sb-bsd-sockets :sb-gray :sb-mop 
    :sb-debug :std/defsys))
