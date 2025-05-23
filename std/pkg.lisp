;;; std/pkg.lisp --- Standard Packages

;;

;;; Code:
(pkg:defpkg :std-int
  (:use :cl)
  (:use-reexport :std/named-readtables :std/defpkg))

(in-package :std-int)

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
   :gensymify* :fboundp! :vboundp!))

(defpkg :std/list
  (:use :cl)
  (:shadowing-import-from :sb-int 
   :ensure-list :recons :memq :assq
   :proper-list-of-length-p :proper-list-p :singleton-p)
  (:import-from :std/sym :with-gensyms)
  (:shadow :group)
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
   :zip-list :zip-tree
   :zipsym
   :ziprm
   :pairs
   :nconsc
   :cart :mapcart
   :cart-case :cart-ecase
   :cart-typecase :cart-etypecase
   :recursive-append :list-dimensions
   :maptree :maptree-if
   :let-binding-transform
   :ensure-list :recons :memq :assq
   :circular-list :circular-list-p :circular-tree-p :merge!
   :sort!
   :set-equal
   :dcons :dpush
   :dpop :dlist :drdc :dcdr :dcar :dappendf
   :topological-sort :match-lambda-lists))

(defpackage :std/prim
  (:use :cl :std/list)
  (:import-from :std/sym :symb :with-gensyms)
  (:import-from :std/named-readtables :parse-body)
  (:export 
   :g!-symbol-p
   :defmacro/g!
   :o!-symbol-p
   :o!-symbol-to-g!-symbol
   :defmacro!
   :unquote-args
   :defun!
   :definline
   :with-optimization
   :macrofy
   :with-marking
   :using-gensyms
   :binding-gensyms))

(defpackage :std/condition
  (:use :cl)
  (:shadowing-import-from :asdf :error-name)
  (:import-from :std/list :flatten :removef)
  (:export
   :*error-message*
   :*handlers*
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
   :unknown-argument
   :error-name
   :error-kind
   :missing-argument
   :error-item
   :error-items
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
   :missing-methods
   :conflicting-arguments
   :unknown-token
   :condition-handler
   :wrapped-condition
   :wrapped-condition-value
   :wrap-condition
   :wrapped-error
   :wrap-error))
   
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
  (:import-from :sb-int :power-of-two-ceiling)
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
   ;; num/math
   :power-of-two-ceiling
   :power-of-two-p
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
  (:import-from :std/sym :with-gensyms)
  (:export
   :read-lisp-until-end
   :read-until-end
   ;; stream
   :copy-stream
   :wrapped-stream
   :wrapped-character-input-stream
   :wrapped-character-output-stream
   :counting-character-input-stream
   :prefixed-character-output-stream
   :stream-of :char-count-of :line-count-of :col-count-of
   :prev-col-count-of :col-index-of :write-prefix
   :prefix-of
   :with-input-from-file :with-output-to-file))

(defpkg :std/array
  (:use :cl)
  (:import-from :sb-ext :maybe-inline)
  (:import-from :std/prim :definline)
  (:export :copy-array :signed-array-length :array-shift 
   :vector-push-extend-position :vector-pop-position
   :vectorify :make-array-allocator
   :vector-foldl :vector-foldr
   :vector-map-foldl :vector-map-foldr
   :vector-max :vector-min
   :vector-eq
   :vector-to-list :copy-vector-to-list
   :modproj))

(defpkg :std/hash-table
  (:use :cl)
  (:nicknames :std/ht)
  (:recycle :sb-int)
  (:import-from :sb-int :ensure-gethash)
  (:export :hash-table-alist
   :maphash-keys :hash-table-keys
   :maphash-values :hash-table-values
   :alist-hash-table :plist-hash-table :hash-table-plist :ensure-gethash
   :pophash))

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
  (:import-from :std/prim :defmacro!)
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
  (:use :cl :std/prim)
  (:import-from :std/sym :symb :mkstr :make-gensym-list :with-gensyms :symbolicate :keywordicate)
  (:import-from :sb-int :make-macro-lambda :parse-lambda-list)
  (:import-from :std/curry :compose)
  (:import-from :std/named-readtables :in-readtable :parse-body)
  (:import-from :std/list :flatten :recursive-append :zip-tree :group)
  (:import-from :std/prim :defmacro! :defun! :defmacro/g! :g!-symbol-p :o1-symbol-to-g!-symbol)
  (:export
   :make-macro-lambda
   :parse-lambda-list
   :once-only
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
   :defvar-unbound
   :def!
   :eval-always
   :compile-and-eval
   ;; ana
   :awhen
   :acond
   :acase
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
   :get-pandoric
   :with-pandoric
   ;; :pandoric-hotpatch
   :pandoric-recode
   :plambda
   :pandoric-eval
   :with-collectors
   :collecting
   :switch
   :eswitch
   :cswitch
   :xor
   :ifret
   :letv*
   :lety
   :lety*
   :defunits :unit-of-distance 
   :distance-designator))

;; (reexport-from :sb-c
;; 	       :include '(:define-source-transformation
;; 			  :parse-eval-when-situations
;; 			  :source-location))

(defpkg :std/sys
  (:use :cl)
  (:shadowing-import-from :sb-kernel :get-lisp-obj-address :with-pinned-objects :unbound-marker-p :generation-of)
  (:shadowing-import-from :sb-vm :list-allocated-objects)
  (:use-reexport :sb-cltl2)
  (:recycle :sb-assem)
  (:shadowing-import-from :sb-c :lexenv-user-data :lexenv-find :make-null-lexenv)
  (:shadowing-import-from :sb-c :define-vop)
  (:shadowing-import-from :sb-c :define-source-transform :parse-eval-when-situations :source-location)
  (:recycle :sb-sys)
  (:import-from :sb-ext :maybe-inline :defglobal :define-load-time-global)
  (:import-from :std/sym :with-gensyms)
  (:import-from :std/list :appendf)
  (:import-from :sb-assem :*backend-instruction-set-package*)
  (:import-from :sb-impl :*logical-hosts* :make-logical-host :logical-host)
  (:import-from :std/macs :if-let :defmacro!)
  (:export
   :.i ;; alias for *inspected*
   :maybe-inline
   :defglobal :define-load-time-global
   :register-project-directory
   :define-vop
   :define-source-transform
   :parse-eval-when-situations 
   :source-location
   :lexenv-user-data
   :lexenv-find
   :make-null-lexenv
   :revive-image
   :64-bit-p :32-bit-p
   :*logical-hosts*
   :save-shared-objects
   :make-logical-host
   :hooks
   :*default-package*
   :*default-arena-size*
   :current-lisp-implementation
   :current-machine
   :list-package-symbols
   :list-all-symbols
   :do-internal-symbols
   :package-symbols
   :package-symbol-names
   :define-logical-pathname
   :logical-host-names
   :save-lisp-tree-shake-and-die
   :save-lisp-and-live
   :forget-shared-object
   :forget-shared-objects
   :compile-lisp
   :without-fp-traps
   :little-endian-p
   :cpuid
   :cpu-vendor
   :get-real-time-seconds 
   :time-remaining 
   :with-countdown))

(defpkg :std/bit
  (:use :cl)
  (:import-from :std/type :octet :octet-vector)
  (:mix :sb-sys)
  (:export
   :read-n-bytes
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
   :make-octets
   :octets))

(defpkg :std/serde
  (:use :cl)
  (:import-from :std/named-readtables :parse-body)
  (:import-from :std/condition :deferror)
  (:import-from :std/macs :when-let :eval-always :once-only)
  (:import-from :std/sym :symbolicate :with-gensyms)
  (:import-from :std/type :octet-vector)
  (:export :define-io
   :*simple-lisp-objects* :*lisp-objects* :serializable-p :deserializable-p
   :ser :de :serialize :deserialize
   :serde-condition :serde-error :serializer-error :deserializer-error
   :serde))

(defpkg :std/alien
  (:use :cl :sb-alien)
  (:import-from :std/sym :symbolicate :with-gensyms)
  (:import-from :std/sys :little-endian-p :32-bit-p)
  (:import-from :std/bit :make-octets)
  (:import-from :std/type :octet-vector :octet)
  (:import-from :std/serde :define-io)
  (:import-from :sb-alien :sap+)
  (:export
   :with-vector-sap
   :setfa
   :double-array-pointer
   :float-array-pointer
   :octet-vector-pointer
   :copy-c-string
   :clone-strings
   :clone-octet-vector-list
   :clone-integer-list
   :clone-octet-vector-list*
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
   :c-strings-to-string-list
   :list-all-shared-objects
   :read-alien-signed-byte-32 :read-alien-fixnum
   :read-alien-signed-byte-64 :read-alien-unsigned-byte-32
   :read-alien-unsigned-byte-64 :read-alien-single-float
   :read-alien-double-float :write-alien-signed-byte-32
   :write-alien-fixnum :write-alien-unsigned-byte-32
   :write-alien-signed-byte-64
   :write-alien-unsigned-byte-64 :write-alien-single-float
   :write-alien-double-float :offset-char-pointer
   :num-cpus
   :*cpus*
   :alien-or-lisp-octets
   :foreign-alloc
   :foreign-free
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
   :free
   :push-sap
   :push-sap*
   :pull-sap
   :pull-sap*
   :defar))

(defpkg :std/meta
  (:use :cl :sb-pcl)
  (:use-reexport :sb-mop)
  (:import-from :std/sym :symb :make-keyword :with-gensyms)
  (:import-from :sb-ext :without-package-locks)
  (:import-from :std/macs :eval-always)
  (:import-from :std/prim :definline)
  (:shadow :reset)
  (:export :list-slot-values-using-class
   :list-class-methods :list-class-slots :ensure-finalized 
   :subclassp :write-object :start :started-p
   :stop :stopped-p :shutdown :reset
   :defaccessor :defaccessor* :defmethods :defclass!
   :data :name :tags :shallow-copy-object
   :exec :copy-object :safe-superclasses :run-object
   :slot-boundp* :slot-values
   :explore :explain :with-fslots))

(defpkg :std/spin
  (:use :cl)
  (:import-from :sb-ext :cas)
  (:export :spin-queue :make-spin-queue :push-spin-queue
   :pop-spin-queue :peek-spin-queue :spin-queue-count :spin-queue-empty-p
   :make-spin-lock))

(defpkg :std/seq
  (:use :cl)
  (:import-from :sb-thread :with-mutex :make-mutex :condition-notify :make-waitqueue :condition-wait)
  (:shadow :queue :make-queue :queue-count :queue-empty-p)
  (:import-from :sb-int :collect)
  (:import-from :std/prim :definline)
  (:import-from :std/array :signed-array-length)
  (:import-from :std/sym :symbolicate)
  (:import-from :std/type :array-length :array-index)
  (:import-from :std/sys :get-internal-time-seconds :time-remaining :with-countdown)
  (:export :take :starts-with-subseq :ends-with-subseq
   :split-sequence :split-sequence-if :split-sequence-if-not :starts-with-p
   :starts-with-one-of-p :copy-n
   :basic-queue :raw-queue-count :raw-queue :make-raw-queue
   :pop-raw-queue :peek-raw-queue :raw-queue-empty-p :raw-queue-full-p
   :raw-queue-capacity :cons-queue :push-cons-queue
   :pop-cons-queue :make-cons-queue :peek-cons-queue :cons-queue-empty-p
   :push-queue :push-queue* :pop-queue :pop-queue* :peek-queue :peek-queue*
   :queue-count :queue-count* :queue-empty-p :queue-empty-p* :queue-full-p :queue-full-p*
   :try-pop-queue :try-pop-queue* :call-with-queue-lock :with-queue-lock
   :queue :make-queue
   :accumulated
   :accumulate
   :accumulator
   :max-accumulator))

(defpkg :std/thread
  (:use :cl)
  (:shadowing-import-from :std/seq :queue-empty-p :queue :queue-count :make-queue)
  (:use :sb-thread :std/meta :std/macs :std/sym :std/type :std/spin :std/condition :std/seq)
  (:import-from :std/list :flatten)
  (:import-from :std/curry :ensure-function)
  (:import-from :std/macs :eval-always)
  (:use-reexport :sb-thread)
  (:import-from :std/macs :if-let :eval-always)
  (:import-from :std/list :deletef)
  (:export
   :*worker-class*
   :%worker
   :*worker*
   :*worker-kernel*
   :*pool-kernel*
   :%thread
   :*thread-pool*
   :find-thread-pool
   :run-thread
   :std-thread-error :thread-support-p
   :print-top-level :println-top-level
   :find-thread-by-id :thread-id-list
   :timed-join-thread :kill-thread
   :wait-for-threads :workers
   :hang :finish-threads
   :make-workers
   :make-oracle
   :kill-worker
   :join-worker
   :start-worker
   :run-worker
   :with-default-special-bindings
   :worker-thread
   :worker-count
   :worker-count*
   :worker-index
   :worker-index*
   :oracle 
   :oracle-id :find-thread
   :make-threads :with-threads 
   :thread-count :dump-thread
   :channel
   :channel-pool
   :channel-queue
   :thread-pool :workers
   :make-thread-pool :end-thread-pool
   :make-worker :designate-oracle
   :condition-wait*
   :sync-message
   :with-sync-message
   :lock
   :schedule
   :+standard-io-bindings+
   :*default-special-bindings*
   :*kernel*
   :kernel
   :check-thread-pool :check-kernel
   :*oracle-table*
   :*worker-threads*
   :*super-threads*
   :compute-special-bindings))

(defpkg :std/task
  (:use :cl :std/thread :sb-concurrency :std/meta :std/spin)
  (:import-from :std/thread :%make-thread)
  (:export
   :push-worker
   :task-schedule
   :status
   :jobs
   :tasks
   :results
   :kill-workers
   :start-task-worker
   :start-task-workers
   :pop-worker
   :*task-class*
   :*task-priority*
   :*tasks*
   :*jobs*
   :*stages*
   :*task*
   :*result*
   :define-task-kernel
   :task :job :task-pool :scheduled-task
   :make-job
   :jobp :taskp :task :with-task-pool))

(defpkg :std/async
  (:use :cl :std/task :std/thread)
  (:import-from :std/macs :with-gensyms :when-let)
  (:export :future :promise :await 
   :fulfill :fulfilledp :while-waiting-for))

(defpkg :std/par
  (:use :cl :std/task :std/thread :std/macs :std/sym)
  (:export))

(defpkg :std/rand
  (:use :cl)
  (:import-from :std/type :octet)
  (:export
   :random-elt
   :random-ref
   :random-char
   :random-chars
   :random-bytes
   :random-booleans
   :random-do))

(defpkg :std/fmt
  (:use :cl)
  (:import-from :std/list :group :ensure-cons)
  (:import-from :std/rand :random-booleans)
  (:import-from :sb-ext :*print-circle-not-shared* :*suppress-print-errors*)
  (:import-from :sb-impl :prin1-to-line)
  (:shadowing-import-from :uiop :println)
  (:export :printer-status :fmt-row :format-sxhash 
   :iprintln :fmt-tree :println :human-readable-size 
   :print-slots :format-slots :*print-slot-indent* :make-bitmap
   :with-bitmap :set-pixel :outside-bounds :draw
   :pattern-to-bitmap :draw-border :draw-circle :bullseye
   :moire :draw-line :sunbeam :fill-bitmap 
   :draw-filled-circle :sun :peace :with-comic-strip
   :plot-function :print-table :print-heading :print-in-box
   :print-boxed :smile :draw-one-in-chance :draw-chance))

(defpkg :std/path
  (:use :cl)
  (:import-from :uiop :directory-files :subdirectories)
  (:export
   :directory-files
   :subdirectories
   :path
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
   :+wildfile+ :+pathsep+ :set-pathname-suffix :*tmp-suffix*
   :tmpize-pathname
   :with-directory
   :with-tmp
   :walk-directory))

(defpkg :std/os
  (:use :cl :sb-alien)
  (:import-from :std/macs :with-gensyms)
  (:import-from :sb-posix :tcgetattr :tcsetattr 
   :termios :termios-cc :termios-cflag :termios-iflag 
   :termios-oflag :termios-lflag)
  (:import-from :std/alien :defar)
  (:export
   :sudo-p
   :user-info
   :list-all-users
   :list-all-groups
   :with-umask
   :with-fd
   :cfmakeraw
   :termios-iflag
   :termios-oflag
   :termios-lflag
   :termios-cflag
   :cc
   :+tiocgwinsz+
   :+tiocswinsz+
   :+tiocnotty+
   :+tcsanow+
   :+tcsaflush+
   :+tcsadrain+
   :+opost+
   :*user*
   :*xdg-user-dirs*
   :xdg-user-dir
   :xdg-base-dir
   :termios
   :winsize
   :isatty
   :tcgetattr
   :tcsetattr
   :tcgetattr*
   :tcsetattr*
   :*xdg-base-dirs*
   :init-xdg-user-dirs
   :init-xdg-base-dirs
   :relative-pathname-p
   :absolute-pathname-p
   :unmerge-pathnames
   :current-directory
   :with-directory-iterator
   :file-kind))

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
   :file-timestamp
   :*hidden-paths*
   :hidden-path-p
   :find-files
   :count-file-lines
   :probe-merge-file
   :probe-directory
   :move-file))

(defpkg :std/pipe
  (:use :cl :std/array)
  (:import-from :std/condition :required-argument :invalid-item :invalid-argument)
  (:import-from :std/sym :with-gensyms)
  (:import-from :std/macs :when-let :eval-always :once-only)
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
   :defpipe :make-pipe :simple-message :message-content
   :defpipe*))

(defpkg :std/string
  (:use :cl)
  (:use-reexport :sb-unicode)
  (:export
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
   :detabify))

(defpkg :std
  (:use :cl :sb-unicode :cl-ppcre :sb-mop :sb-c :sb-thread :sb-alien :sb-gray)
  (:use-reexport :std/named-readtables :std/defpkg :std/condition
   :std/sym :std/list :std/type :std/num :std/prim
   :std/stream :std/curry :std/array :std/hash-table
   :std/alien :std/meta :std/thread :std/task
   :std/macs :std/bit :std/fmt :std/path
   :std/os :std/file :std/string :std/seq
   :std/sys :std/readtable :std/pipe :std/serde
   :std/rand :std/async :std/par :std/spin)
  (:export :*std-packages*))

(defpkg :std-user
  (:use :cl :cl-user :sb-ext :std
   :std-int :sb-alien :sb-thread :sb-bsd-sockets
   :sb-gray :sb-mop :sb-debug))

(pkg:define-lisp-package :std)

(in-package :std)
(defvar *std-packages*
  '(:std/named-readtables :std/defpkg :std/condition
    :std/sym :std/list :std/type :std/num
    :std/stream :std/curry :std/array :std/hash-table
    :std/alien :std/meta :std/thread :std/task
    :std/macs :std/bit :std/fmt :std/path
    :std/os :std/file :std/string :std/seq
    :std/sys :std/readtable :std/pipe :std/serde
    :std/rand :std/async :std/par :std/spin))

(asdf:register-system-packages "STD" *std-packages*)
