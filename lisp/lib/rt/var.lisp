;;; var.lisp --- Test Variables

;; 

;;; Code:
(in-package :rt)

;;; Vars
(defvar *test-opts* '(optimize sb-c::instrument-consing))
(defvar *compile-tests* nil
  "When nil do not compile tests. With a value of t, tests are compiled
with default optimizations else the value is used to configure
compiler optimizations.")
(defvar *catch-test-errors* t "When non-nil, cause errors in a test to be caught.")
(defvar *test-suffix* "-TEST" "A suffix to append to every `test' defined with `deftest'.")
(defvar *test-suite-list* nil "List of available `test-suite' objects.")
(defvar *test-suite* nil "A 'test-suite-designator' which identifies the current `test-suite'.")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *default-test-suite-name* "default"))
(declaim (type (or stream boolean string) *test-input*))
(defvar *test-input* nil "When non-nil, specifies an input stream or buffer for `*testing*'.")
(defvar *testing* nil "Testing state var.")
(defvar *default-tmp-directory* #P"/tmp/")
(defvar *tmp* *default-tmp-directory*)
;; TODO 2024-08-31:
(defvar *test-on-definition* nil
  "Special variable indicating whether to run tests as soon as they are defined.")
