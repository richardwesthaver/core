;;; pkg.lisp --- regression testing packages

;; Regression Testing framework. inspired by PCL, the original CMUCL
;; code, and the SBCL port.

;;; Commentary:

;; - :rt https://www.merl.com/publications/docs/TR91-04.pdf Chapter 1
;; - :com.gigamonkeys.test https://github.com/gigamonkey/monkeylib-test-framework
;; - :sb-rt https://github.com/sbcl/sbcl/blob/master/contrib/sb-rt/rt.lisp

;; This package is intended to provide a modernized Lisp testing
;; library with features found in some of the test frameworks listed
;; below.

;; - :it.bese.fiveam https://github.com/lispci/fiveam
;; - :try https://github.com/melisgl/try
;; - :rove https://github.com/fukamachi/rove

;;; TODO:
#|

- [ ] benchmark support: do-bench, test-count, 

- [ ] fixtures api

- [ ] profiling 
|#
;;; Code:
(in-package :std-user)

(defpackage :rt
  (:use 
   :cl :std :sxp :log :obj/ast
   :sb-aprof)
  (:export
   :test-error
   :*compile-tests*
   :*catch-test-errors*
   :*test-suffix*
   :*default-test-suite-name*
   :*test-suite*
   :*test-suite-list*
   ;;  TODO 2023-09-04: :*test-profiler-list* not yet
   :*testing*
   :test-declare
   :test-policy
   :test-suite-designator
   :check-suite-designator
   :make-test
   :make-suite
   :test-name=
   :do-test
   :do-tests
   :reset-tests
   :continue-testing
   :with-test-env
   :%test-bail
   :%test-result
   :make-test-result
   :ensure-suite
   :fixture
   :fixture-prototype
   :make-fixture-prototype
   :make-fixture
   :with-fixture
   :tmp-fixture
   :test-result
   :test-fn
   :test-pass-p
   :test-fail-p
   :test-skip-p
   :test-failed
   :fail!
   :is
   :signals
   :deftest
   :defsuite
   :in-suite
   :eval-test
   :compile-test
   :locked-tests
   :push-test
   :pop-test
   :delete-test
   :find-test
   :find-suite
   :do-suite
   :test-object
   :test
   :test-fixture
   :test-suite
   :tests
   :test-form
   :test-results
   :*tmp*
   :*default-tmp-directory*
   :with-tmp-directory
   :with-tmp-file
   :isnt
   :is=
   :iseq
   :iseql
   :isequalp
   :isequal
   :isand
   :isempty
   :istype
   :issubtype
   :issubclass
   :iszero
   :isevery
   :issome
   :islist
   :test-fixtures
   :*fx*
   :*fixtures*
   :*test-policy*
   :is>
   :is<
   :is>=
   :is<=
   :isor
   :run-all-tests
   :run-all-tests*))

(defpackage :rt/bench
  (:nicknames :bench)
  (:use :cl :std :log :rt)
  (:export
   :*bench-count*
   :defbench
   :do-bench
   :bench
   :time-total))

(defpkg :rt/cover
  (:nicknames :cover)
  (:use :cl :std :log :rt)
  (:use-reexport :sb-cover)
  (:export
   :enable-coverage :disable-coverage
   :*coverage-directory*
   :coverage-report))

(defpackage :rt/tracing
  (:nicknames :tracing)
  (:use :cl :std :log :rt)
  (:export
   :start-tracing
   :stop-tracing
   :with-tracing
   :save-report
   ;; Extra utility
   :package-symbols-except))

(defpackage :rt/flamegraph
  (:nicknames :flamegraph)
  (:use :cl :std :log :rt)
  (:export :with-flamegraph))

(defpackage :rt/fuzz
  (:nicknames :fuzz)
  (:use :cl :std :log :rt)
  (:export :fuzzer
           :fuzz
           :fuzz*
           :fuzz-generator
           :fuzz-state))
