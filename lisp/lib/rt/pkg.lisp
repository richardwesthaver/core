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
(require 'sb-cover)
(defpackage :rt
  (:use 
   :cl :std :sxp :log
   :sb-aprof)
  (:export
   :test-error
   :*test-opts*
   :*compile-tests*
   :*catch-test-errors*
   :*test-suffix*
   :*default-test-suite-name*
   :*test-suite*
   :*test-suite-list*
   ;;  TODO 2023-09-04: :*test-profiler-list* not yet
   :*testing*
   :random-elt
   :random-ref
   :random-char
   :random-chars
   :random-bytes
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
   :test-fixture
   :fixture-prototype
   :make-fixture-prototype
   :make-fixture
   :with-fixture
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
   :test-name
   :tests
   :test-form
   :test-results
   :*tmp*
   :*default-tmp-directory*
   :with-tmp-directory
   :with-tmp-file))

(defpackage :rt/bench
  (:nicknames :bench)
  (:use :cl :std :log :rt)
  (:export
   :*bench-count*
   :defbench
   :do-bench))

(uiop:define-package :rt/cover
  (:nicknames :cover)
  (:use :cl :std :log :rt)
  (:export
   :with-coverage :start-coverage :stop-coverage
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
  (:use :cl :std :log :rt :sb-sprof)
  (:export :save-flamegraph))

(defpackage :rt/fuzz
  (:nicknames :fuzz)
  (:use :cl :std :log :rt)
  (:export :fuzzer
           :fuzz
           :fuzz*
           :fuzz-generator
           :fuzz-state))
