;;; var.lisp --- Test Variables

;; 

;;; Code:
(in-package :rt)

;;; Vars
(defvar *test-policy* '(optimize sb-c:instrument-consing sb-c:store-coverage-data (debug 3)))
;; TODO 2024-08-31:
(defvar *test-on-def* nil
  "Special variable indicating whether to run tests as soon as they are defined.")
(defvar *compile-tests* nil
  "When nil do not compile tests. With a value of t, tests are compiled
with default optimizations else the value is used to configure
compiler optimizations.")
(defvar *catch-test-errors* t "When non-nil, cause errors in a test to be caught.")
(defvar *test-suffix* "-TEST" "A suffix to append to every `test' defined with `deftest'.")
(defvar *test-suites* nil "List of available `test-suite' objects.")
(defvar *test-suite* nil "A 'test-suite-designator' which identifies the current `test-suite'.")
(defvar-unbound *fx* "The currently bound fixture. Should only be used inside the body of WITH-FIXTURE.")
(defvar *fixtures* nil "The list of fixtures available in the current *TEST-SUITE*.")
(defvar *default-test-suite-name* "default")
(declaim (type (or stream boolean string) *test-input*))
(defvar *test-input* nil "When non-nil, specifies an input stream or buffer for `*testing*'.")
(defvar *test-output* nil "When non-nil, specifies an output stream or buffer for `*testing*'.")
(defvar *testing* nil "Testing state var.")
(defvar *default-tmp-directory* #P"/tmp/" "Default temp pathname directory.")
(defvar *tmp* *default-tmp-directory* "The current temp directory")
(defvar *coverage-directory* #P"/tmp/rt/")
(defconstant +test-tag+ '%test)
