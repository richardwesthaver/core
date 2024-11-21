;;; std/pkg.lisp --- Standard Packages

;;

;;; Code:
(pkg:defpkg :std-int
  (:use :cl)
  (:use-reexport :std/named-readtables :std/defpkg))

(in-package :std-int)

(defpackage :std/condition
  (:use :cl)
  (:shadowing-import-from :asdf :error-name)
  (:export    ;; err
   :*error-message*
   :std-error :error-message
   :define-error-reporter
   :deferror
   :nyi!
   :required-argument
   :ignore-some-conditions
   :simple-style-warning
   :simple-reader-error
   :simple-parse-error
   :simple-program-error
   :circular-dependency
   :circular-dependency-items
   :unknown-argument
   :error-name
   :error-kind
   :missing-argument
   :missing-argument-command
   :error-item
   :error-reason
   :invalid-argument
   :unwind-protect-case
   :def-simple-error-reporter
   :std-warning
   :defwarning
   :def-simple-warning-reporter
   :def-warning-reporter
   :meta-condition
   :missing-method
   :missing-methods))

(defpackage :std/sym
  (:use :cl)
  (:shadowing-import-from :sb-int
   :with-unique-names :symbolicate :package-symbolicate :keywordicate :gensymify*
   :gensymify)
  (:export
   :ensure-symbol
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
   :gensymify* :fboundp! :vboundp! :quoty))

(defpkg :std/list
  (:use :cl)
  (:shadowing-import-from :sb-int 
   :ensure-list :recons :memq :assq
   :ensure-list :proper-list-of-length-p :proper-list-p :singleton-p)
  (:import-from :std/sym :symb)
  (:import-from :std/named-readtables :parse-body)
  (:export
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
   :let-binding-transform
   :ensure-list :recons :memq :assq
   :circular-list :circular-list-p :circular-tree-p :merge!
   :sort!
   :set-equal))
   
(defpkg :std/type
  (:use :cl)
  (:import-from :std/sym :format-symbol :with-gensyms)
  (:import-from :std/list :ensure-car)
  (:shadowing-import-from :sb-ext :word)
  (:export :+default-element-type+
   :array-index :array-length
   #:negative-double-float
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
   :negative-integer
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
   :coercef
   :octet
   :octet-vector
   :octet-vector-p
   #:positive-integer-p
   #:positive-long-float
   #:positive-long-float-p
   #:positive-rational-p
   :of-type
   :type=
   :word))

(defpkg :std/num
  (:use :cl)
  (:export
   ;; num/parse
   :parse-number
   :parse-real-number
   :parse-positive-real-number
   :invalid-number
   :invalid-number-value
   :invalid-number-reason
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
   :clamp
   :gaussian-random
   :iota
   :map-iota
   :lerp
   :mean
   :median
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
  (:export
   ;; stream
   :copy-stream
   :wrapped-stream
   :wrapped-character-input-stream
   :wrapped-character-output-stream
   :counting-character-input-stream
   :prefixed-character-output-stream
   :stream-of :char-count-of :line-count-of :col-count-of
   :prev-col-count-of :col-index-of :write-prefix
   :prefix-of))

(defpkg :std/array
  (:use :cl)
  (:export :copy-array :signed-array-length :array-shift 
   :vector-push-extend-position :vector-pop-position))

(defpkg :std/hash-table
  (:use :cl)
  (:nicknames :std/ht)
  (:recycle :sb-int)
  (:import-from :sb-int :ensure-gethash)
  (:export :hash-table-alist
   :maphash-keys :hash-table-keys
   :maphash-values :hash-table-values
   :alist-hash-table :plist-hash-table :hash-table-plist :ensure-gethash))

(defpkg :std/curry
  (:use :cl)
  (:import-from :std/sym :make-gensym-list)
  (:export
   :ensure-function
   :ensure-functionf
   :disjoin
   :conjoin
   :compose
   :multiple-value-compose
   :curry
   :rcurry))

(defpkg :std/readtable
  (:use :cl)
  (:import-from :std/named-readtables :defreadtable)
  (:import-from :std/curry :curry :rcurry :compose)
  (:import-from :std/sym :symb)
  (:import-from :std/list :defmacro!) ;; kludge
  (:export
   ;; readtable
   :|#"-reader|
   :|#`-reader|
   :|#f-reader|
   :|#$-reader|
   :segment-reader
   :match-mode-ppcre-lambda-form
   :subst-mode-ppcre-lambda-form
   :|#~-reader|
   :_))

(defpkg :std/macs
  (:use :cl)
  (:import-from :std/sym :symb :mkstr :make-gensym-list :once-only :with-gensyms)
  (:import-from :std/curry :compose)
  (:import-from :std/named-readtables :in-readtable :parse-body)
  (:import-from :std/list :flatten :defmacro!)
  (:export
   :define-class
   :defclass*
   :dlet
   :named-lambda
   :nested-loop
   :g!-symbol-p
   :defmacro/g!
   :o!-symbol-p
   :o!-symbol-to-g!-symbol
   :defmacro!
   :defun!
   :dlambda
   :until
   :fact
   :choose
   :make-tlist
   :tlist-left
   :tlist-right
   :tlist-empty-p
   :tlist-add-left
   :tlist-add-right
   :tlist-rem-left
   :tlist-update
   :build-batcher-sn
   :sortf
   :dollar-symbol-p
   :if-match
   :when-match
   :once-only
   :destructuring-case
   :destructuring-ccase
   :destructuring-ecase
   :when-let
   :when-let*
   :if-let
   :if-let*
   :if*
   :define-constant
   :defvar-unbound
   :def!
   :eval-always
   ;; ana
   :awhen
   :acond
   :alambda
   :nlet-tail
   :alet%
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
   :get-pandoric
   :with-pandoric
   :pandoric-hotpatch
   :pandoric-recode
   :plambda
   :pandoric-eval
   :with-collectors
   :collecting
   :switch
   :eswitch
   :cswitch
   :xor
   :ifret))

(defpkg :std/sys
  (:use :cl)
  (:shadowing-import-from :sb-kernel :get-lisp-obj-address :with-pinned-objects :unbound-marker-p :generation-of)
  (:shadowing-import-from :sb-vm :list-allocated-objects)
  (:recycle :sb-assem)
  (:recycle :sb-sys)
  (:import-from :sb-assem :*backend-instruction-set-package*)
  (:import-from :sb-impl :*logical-hosts*)
  (:import-from :std/macs :if-let)
  (:export
   :.i ;; alias for *inspected*
   :64-bit-p :32-bit-p
   :hooks
   :*default-arena-size*
   :current-lisp-implementation
   :current-machine
   :list-package-symbols
   :package-symbols
   :package-symbol-names
   :append-logical-hosts
   :add-logical-pathname-translation
   :save-lisp-tree-shake-and-die
   :save-lisp-and-live
   :forget-shared-object
   :forget-shared-objects
   :compile-lisp
   :without-fp-traps
   :little-endian-p
   :cpuid
   :cpu-vendor))

(defpkg :std/bit
  (:use :cl)
  (:import-from :std/type :octet :octet-vector)
  (:export
   :make-bits
   :sign-bit
   :different-signs-p
   :mortify-bits
   :int-list-bits
   :aref-bit
   :make-bit-vector
   :logbit
   :bitfield
   :bitfield-slot-name
   :bitfield-slot-start
   :bitfield-slot-end
   :bitfield-slot-size
   :bitfield-slot-reader
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
   :octet-vector-to-hex-string
   :octets-to-integer
   :integer-to-octets
   :octets-to-integer-le
   :integer-to-octets-le
   :read-little-endian
   :write-little-endian
   :hexchar-to-int
   :make-octets))

(defpkg :std/serde
  (:use :cl)
  (:import-from :std/named-readtables :parse-body)
  (:import-from :std/macs :when-let :once-only)
  (:import-from :std/sym :symbolicate :with-gensyms)
  (:import-from :std/type :octet-vector)
  (:export :define-serde :*lisp-objects*))

(defpkg :std/alien
  (:use :cl :sb-alien)
  (:import-from :std/sym :symbolicate :with-gensyms)
  (:import-from :std/sys :little-endian-p :32-bit-p)
  (:import-from :std/bit :make-octets)
  (:import-from :std/type :octet-vector :octet)
  (:import-from :std/serde :define-serde)
  (:import-from :sb-alien :sap+)
  (:export
   :setfa
   :copy-c-string
   :clone-strings
   :octets-to-alien-array
   :with-alien-slots
   :clone-octets-to-alien
   :octets-to-alien
   :clone-octets-from-alien
   :foreign-int-to-integer
   :foreign-int-to-bool
   :bool-to-foreign-int
   :define-alien-enum
   :define-opaque
   :shared-object-name
   :define-alien-loader
   :c-string-to-string-list
   :list-all-shared-objects
   :read-alien-signed-byte-32 :read-alien-fixnum
   :read-alien-signed-byte-64 :read-alien-unsigned-byte-32
   :read-alien-unsigned-byte-64 :read-alien-float
   :read-alien-double :write-alien-signed-byte-32
   :write-alien-fixnum :write-alien-unsigned-byte-32
   :write-alien-signed-byte-64
   :write-alien-unsigned-byte-64 :write-alien-float
   :write-alien-double-float :offset-char-pointer
   :num-cpus
   :*cpus*
   :alien-or-lisp-octets
   :read-alien
   :write-alien
   :loff-t
   :pid-t
   :uid-t
   :gid-t
   :memset
   :memcpy
   :posix-memalign
   :timeval
   :timespec
   :sap
   :push-sap
   :push-sap*
   :pull-sap
   :pull-sap*))

(defpkg :std/meta
  (:use :cl :sb-mop :sb-pcl)
  (:import-from :std/sym :symb :make-keyword)
  (:import-from :sb-ext :without-package-locks)
  (:import-from :std/macs :eval-always)
  (:shadow :reset)
  (:export :list-slot-values-using-class
   :list-class-methods :list-class-slots :list-indirect-slot-methods :ensure-finalized 
   :subclassp :write-object :start :started-p 
   :stop :stopped-p :shutdown :reset
   :defaccessor :defaccessor* :defmethods
   :data))

(defpkg :std/thread
  (:use :cl :sb-thread :sb-concurrency :std/meta)
  (:import-from :std/list :flatten)
  (:import-from :std/macs :eval-always)
  (:use-reexport :sb-thread)
  (:export
   :run-thread
   :std-thread-error
   :print-top-level :thread-support-p
   :find-thread-by-id :thread-id-list
   :timed-join-thread :kill-thread
   :wait-for-threads :workers
   :hang :finish-threads
   :make-threads :with-threads 
   :thread-count :dump-thread
   :thread-pool :workers
   :condition-wait*
   :sync-message
   :with-sync-message
   :lock))

(defpkg :std/task
  (:use :cl :std/thread :sb-concurrency)
  (:import-from :std/thread :%make-thread)
  (:import-from :std/macs :if-let :eval-always)
  (:import-from :std/list :deletef)
  (:export
   :spawn-workers
   :make-oracle :make-supervisor
   :oracle 
   :oracle-id :find-thread
   :push-job :push-task
   :push-worker :push-task-result
   :run-object 
   :work
   :pop-job :pop-task
   :tasks
   :results
   :kill-workers
   :kill-worker
   :join-worker
   :worker-thread
   :start-task-worker
   :start-task-workers
   :pop-worker :pop-task-result
   :*task-pool*
   :*tasks*
   :*oracles*
   :*oracle-threads*
   :*worker-threads*
   :*supervisor-threads*
   :*jobs*
   :*stages*
   :*task*
   :*task-result*
   :define-task-kernel
   :*task-kernel*
   :default-task-kernel
   :make-worker
   :make-workers
   :run-tasks
   :run-jobs
   :worker-count
   :init-task-pool
   :make-task-pool
   :start-task-pool :pause-task-pool
   :shutdown-task-pool
   :push-stage :designate-oracle
   :make-task-pool
   :task :job :task-pool
   :stage :task-pool-p
   :job-tasks :make-job
   :job-p :task-object
   :make-task :task-p :task
   :task-pool-oracle :task-pool-jobs
   :task-pool-stages
   :task-pool-workers :task-pool-results
   :with-task-pool))

(defpkg :std/fmt
  (:use :cl)
  (:import-from :std/list :group :ensure-cons)
  (:shadowing-import-from :uiop :println)
  (:export :printer-status :fmt-row :format-sxhash :iprintln :fmt-tree :println))

(defpkg :std/path
  (:use :cl)
  (:export
   :path
   :wild-pathname
   :non-wild-pathname
   :absolute-pathname
   :relative-pathname
   :directory-pathname
   :symlink-pathname
   :symlinkp
   :absolute-directory-pathname
   :+wildfile+ :+pathsep+ :set-pathname-suffix :*tmp-suffix*
   :tmpize-pathname))

(defpkg :std/os
  (:use :cl :sb-alien)
  (:import-from :std/macs :with-gensyms)
  (:export
   :sudo-p
   :list-all-users
   :list-all-groups
   :with-umask
   :with-fd))

(defpkg :std/file
  (:use :cl)
  (:import-from :std/macs :define-constant :once-only :eval-always)
  (:import-from :std/stream :copy-stream)
  (:import-from :std/type :octet :octet-vector :array-index :array-length :+default-element-type+)
  (:export
   :tmpfile
   :dir
   :file
   :file-pathname
   :with-open-files
   :write-stream-into-file
   :write-file-into-stream
   :file=
   :file-size
   :file-size-in-octets
   :octet-vector=
   :file-date
   :file-timestamp
   :directory-path-p
   :*hidden-paths*
   :hidden-path-p
   :directory-path
   :find-files
   :count-file-lines))

(defpkg :std/pipe
  (:use :cl :std/array)
  (:import-from :std/condition :required-argument :invalid-item :invalid-argument)
  (:import-from :std/sym :with-gensyms)
  (:import-from :std/list :removef)
  (:import-from :std/file :file)
  (:export :sink :source :element :filter
   :pipe :msg :print-filter :switch-filter :predicate-filter :bin :predicate
   :element-stream :value :index :resolve-element
   :find-element :find-parent-element :insert-element :withdraw-element
   :remove-element :set-element-id :move-element :message
   :event :buffer :bus :format-message
   :condition-message :message-condition
   :stream-sink :stream-source :file-sink :file-source
   :add-element :insert-element*
   :defpipe :make-pipe :simple-message :message-content))

(defpkg :std/string
  (:use :cl)
  (:export
   :name
   :*omit-nulls*
   :*whitespaces*
   :*tab-width*
   :string-designator
   :ssplit
   :trim
   :collapse-whitespaces
   :make-template-parser
   :string-case
   :detabify))

(defpkg :std/seq
  (:use :cl)
  (:import-from :sb-int :collect)
  (:import-from :std/array :signed-array-length)
  (:export :take :starts-with-subseq :ends-with-subseq
   :split-sequence :split-sequence-if :split-sequence-if-not))

(defpkg :std
  (:use :cl :sb-unicode :cl-ppcre :sb-mop :sb-c :sb-thread :sb-alien :sb-gray :sb-concurrency)
  (:use-reexport :std/named-readtables :std/defpkg :std/condition
   :std/sym :std/list :std/type :std/num
   :std/stream :std/curry :std/array :std/hash-table
   :std/alien :std/meta :std/thread :std/task
   :std/macs :std/bit :std/fmt :std/path
   :std/os :std/file :std/string :std/seq
   :std/sys :std/readtable :std/pipe))

(defpkg :std-user
  (:use :cl :cl-user :sb-ext :std
   :std-int :sb-alien :sb-thread :sb-bsd-sockets
   :sb-gray :sb-mop :sb-debug))

(pkg:define-lisp-package :std)
