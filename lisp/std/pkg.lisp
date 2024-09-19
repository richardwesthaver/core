;;; std/pkg.lisp --- Standard Packages

;;

;;; Code:
(pkg:defpkg :std-int
  (:use :cl)
  (:use-reexport :std/named-readtables :std/defpkg))

(in-package :std-int)

(defpackage :std/condition
  (:use :cl)
  (:export    ;; err
   :std-error :std-error-message
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
   :unknown-argument-name
   :unknown-argument-kind
   :unknown-argument-p
   :missing-argument
   :missing-argument-command
   :missing-argument-p
   :invalid-argument
   :invalid-argument-item
   :invalid-argument-reason
   :invalid-argument-p
   :unwind-protect-case
   :define-simple-error
   :define-simple-error-reporter
   :def-simple-error-reporter
   :std-warning
   :defwarning
   :def-simple-warning-reporter
   :def-warning-reporter))

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
   :gensymify*))

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
  (:export :+default-element-type+
   :array-index :array-length
   :negative-integer :non-negative-integer
   :positive-integer :octet
   :octet-vector))

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
   :decode-uleb128))

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
  (:export :copy-array :signed-array-length))

(defpkg :std/hash-table
  (:use :cl)
  (:nicknames :std/ht)
  (:export :hash-table-alist
   :maphash-keys :hash-table-keys
   :maphash-values :hash-table-values))

(defpkg :std/alien
  (:use :cl :sb-alien)
  (:import-from :std/sym :symbolicate :with-gensyms)
  (:export
   :setfa
   :copy-c-string
   :clone-strings
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
   :num-cpus
   :*cpus*
   :loff-t
   :memset))

(defpkg :std/mop
  (:use :cl :sb-mop :sb-pcl)
  (:import-from :std/sym :symb :make-keyword)
  (:export :list-slot-values-using-class
   :list-class-methods :list-class-slots :list-indirect-slot-methods))
   
(defpkg :std/fu
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

(defpkg :std/macs
  (:use :cl)
  (:import-from :std/sym :symb :mkstr :make-gensym-list :once-only :with-gensyms)
  (:import-from :std/fu :compose)
  (:import-from :std/named-readtables :in-readtable :parse-body)
  (:import-from :std/list :flatten :defmacro!)
  (:export
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
   :xor))

(defpkg :std/thread
  (:use :cl :sb-thread :sb-concurrency)
  (:import-from :std/list :flatten)
  (:use-reexport :sb-thread)
  (:export
   :print-top-level :thread-support-p
   :find-thread-by-id :thread-id-list
   :timed-join-thread :kill-thread
   :wait-for-threads
   :hang :finish-threads
   :make-threads :with-threads 
   :thread-count :dump-thread))

(defpkg :std/task
  (:use :cl :std/thread :sb-concurrency)
  (:import-from :std/thread :%make-thread)
  (:import-from :std/macs :if-let)
  (:export
   :spawn-workers
   :make-oracle :make-supervisor
   :oracle :run-task
   :oracle-id :find-thread
   :push-job :push-task
   :push-worker :push-task-result
   :run-job :run-stage
   :pop-job :pop-task
   :pop-worker :pop-task-result
   :*task-pool*
   :*tasks*
   :*oracles*
   :*workers*
   :*jobs*
   :*stages*
   :define-task-kernel
   :*task-kernel*
   :default-task-kernel
   :make-worker
   :make-workers
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

(defpkg :std/readtable
  (:use :cl)
  (:import-from :std/named-readtables :defreadtable)
  (:import-from :std/fu :curry :rcurry :compose)
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
   :hexchar-to-int))

(defpkg :std/fmt
  (:use :cl)
  (:import-from :std/list :group :ensure-cons)
  (:shadowing-import-from :uiop :println)
  (:export :printer-status :fmt-row :format-sxhash :iprintln :fmt-tree :println))

(defpkg :std/path
  (:use :cl)
  (:export
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
  (:use :cl)
  (:import-from :std/macs :with-gensyms)
  (:export
   :list-all-users
   :list-all-groups
   :with-umask))

(defpkg :std/file
  (:use :cl)
  (:import-from :std/macs :define-constant :once-only :eval-always)
  (:import-from :std/stream :copy-stream)
  (:import-from :std/type :octet :octet-vector :array-index :array-length :+default-element-type+)
  (:export
   :tmpfile
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

(defpkg :std/string
  (:use :cl)
  (:export
   :*omit-nulls*
   :*whitespaces*
   :string-designator
   :ssplit
   :trim
   :collapse-whitespaces
   :make-template-parser
   :string-case))

(defpkg :std/seq
  (:use :cl)
  (:import-from :sb-int :collect)
  (:import-from :std/array :signed-array-length)
  (:export :take :starts-with-subseq :ends-with-subseq
   :split-sequence :split-sequence-if :split-sequence-if-not))

(defpkg :std/sys
  (:use :cl)
  (:shadowing-import-from :sb-kernel :get-lisp-obj-address :with-pinned-objects :unbound-marker-p :generation-of)
  (:shadowing-import-from :sb-vm :list-allocated-objects)
  (:import-from :sb-impl :*logical-hosts*)
  (:export
   :current-lisp-implementation
   :current-machine
   :list-package-symbols
   :package-symbols
   :package-symbol-names
   :append-logical-hosts
   :save-lisp-tree-shake-and-die
   :save-lisp-and-live
   :forget-shared-object
   :forget-shared-objects
   :compile-lisp))

(defpkg :std
  (:use :cl :sb-unicode :cl-ppcre :sb-mop :sb-c :sb-thread :sb-alien :sb-gray :sb-concurrency)
  (:use-reexport :std/named-readtables :std/defpkg :std/condition
   :std/sym :std/list :std/type :std/num
   :std/stream :std/fu :std/array :std/hash-table
   :std/alien :std/mop :std/thread :std/task
   :std/macs :std/bit :std/fmt :std/path
   :std/os :std/file :std/string :std/seq
   :std/sys :std/readtable))

(defpkg :std-user
  (:use :cl :cl-user :sb-ext :std
   :std-int :sb-alien :sb-thread :sb-bsd-sockets
   :sb-gray :sb-mop :sb-debug))

(pkg:define-lisp-package :std)
