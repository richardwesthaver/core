(uiop:define-package :std
    (:use :cl :sb-unicode :cl-ppcre :sb-mop :sb-c :sb-thread :sb-alien)
  (:use-reexport :std/named-readtables)
  (:shadowing-import-from :uiop :println)
  (:shadowing-import-from
   :sb-int 
   :ensure-list :recons :memq :assq :ensure-list 
   :proper-list-of-length-p :proper-list-p :singleton-p
   :with-unique-names :symbolicate :package-symbolicate :keywordicate :gensymify*)
  (:export
   ;; err
   #:nyi!
   #:required-argument
   #:ignore-some-conditions
   #:simple-style-warning
   #:simple-reader-error
   #:simple-parse-error
   #:simple-program-error
   #:circular-dependency
   #:circular-dependency-items
   #:unknown-argument
   #:unknown-argument-name
   #:unknown-argument-kind
   #:unknown-argument-p
   #:missing-argument
   #:missing-argument-command
   #:missing-argument-p
   #:invalid-argument
   #:invalid-argument-item
   #:invalid-argument-reason
   #:invalid-argument-p
   #:unwind-protect-case
   ;; num/float
   :make-float-converters
   :encode-float32
   :decode-float32
   :encode-float64
   :decode-float64
   ;; str
   :*omit-nulls*
   :*whitespaces*
   :string-designator
   :split
   :trim
   :collapse-whitespaces
   ;; fmt
   :printer-status :fmt-row :fmt-sxhash :iprintln :fmt-tree :println
   ;; sym
   #:ensure-symbol
   #:format-symbol
   #:make-keyword
   #:make-slot-name
   #:make-gensym
   #:make-gensym-list
   #:with-gensyms
   #:with-unique-names
   #:symbolicate
   ;; list
   :ensure-car
   :ensure-cons
   :appendf
   :nconcf
   :unionf
   :nunionf
   :reversef
   :nreversef
   :deletef
   :let-binding-transform
   :ensure-list :recons :memq :assq
   :circular-list :circular-list-p :circular-tree-p
   ;; :proper-list-of-length-p :proper-list-p :singleton-p
   ;; thread
   :print-thread-info :print-thread-message-top-level :thread-support-p
:defpkg
   ;; util
   #:find-package* #:find-symbol* #:symbol-call
   #:intern* #:export* #:import* #:shadowing-import* 
   #:shadow* #:make-symbol* #:unintern*
   #:symbol-shadowing-p #:home-package-p
   #:symbol-package-name #:standard-common-lisp-symbol-p
   #:reify-package #:unreify-package #:reify-symbol #:unreify-symbol
   #:nuke-symbol-in-package #:nuke-symbol #:rehome-symbol
   #:ensure-package-unused #:delete-package*
   #:package-names #:packages-from-names #:fresh-package-name 
   #:rename-package-away #:package-definition-form #:parse-defpkg-form
   #:ensure-package
   ;; ana
   :awhen
   :acond
   #:alambda
   #:nlet-tail
   #:alet%
   #:alet
   #:acond2
   #:it
   #:aif
   #:this
   #:self
   ;; pan
   #:pandoriclet
   #:pandoriclet-get
   #:pandoriclet-set
   #:get-pandoric
   #:with-pandoric
   #:pandoric-hotpatch
   #:pandoric-recode
   #:plambda
   #:pandoric-eval
   ;; fu
   :copy-array
   :until
   #:mkstr
   #:symb
   #:group
   #:flatten
   #:fact
   #:choose
   #:g!-symbol-p
   #:defmacro/g!
   #:o!-symbol-p
   #:o!-symbol-to-g!-symbol
   #:defmacro!
   #:defun!
   #:dlambda
   #:make-tlist
   #:tlist-left
   #:tlist-right
   #:tlist-empty-p
   #:tlist-add-left
   #:tlist-add-right
   #:tlist-rem-left
   #:tlist-update
   #:build-batcher-sn
   #:sortf
   #:dollar-symbol-p
   #:if-match
   #:when-match
   #:once-only
   #:destructuring-case
   #:destructuring-ccase
   #:destructuring-ecase
   #:when-let
   #:when-let*
   #:if-let
   #:if-let*
   :if*
   :define-constant
   :def!
   :eval-always
   :merge! :sort!
   :list-slot-values-using-class :list-class-methods :list-class-slots :list-indirect-slot-methods
   :signed-array-length
   :take
   :maphash-keys
   :hash-table-keys
   :maphash-values
   :hash-table-values
   :my-lisp-implementation
   :tmpfile
   :ensure-function
   :ensure-functionf
   :disjoin
   :conjoin
   :compose
   :multiple-value-compose
   :curry
   :rcurry
   :named-lambda
   ;; alien
   ;; :defbytes
   ;; :u1 :u2 :u3 :u4 :u8 :u16 :u24 :u32 :u64 :u128
   ;; :i2 :i3 :i4 :i8 :i16 :i24 :i32 :i64 :i128
   ;; :f16 :f24 :f32 :f64 :f128
   :define-opaque
   :setfa
   :copy-c-string
   :clone-strings
   :clone-octets-to-alien
   :clone-octets-from-alien
   :foreign-int-to-integer :foreign-int-to-bool :bool-to-foreign-int
   ;; readtable
   #:|#"-reader|
   #:|#`-reader|
   #:|#f-reader|
   #:|#$-reader|
   #:segment-reader
   #:match-mode-ppcre-lambda-form
   #:subst-mode-ppcre-lambda-form
   #:|#~-reader|
   :_))
