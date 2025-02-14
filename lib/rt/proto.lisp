;;; proto.lisp --- Test Protocols

;; 

;;; Code:
(in-package :rt)

(defgeneric eval-test (self)
  (:documentation "Eval a `test'."))

(defgeneric compile-test (self &key &allow-other-keys)
  (:documentation "Compile a `test'."))

(defgeneric locked-tests (self)
  (:documentation "Return a list of locked tests in `test-suite' object SELF."))

(defgeneric push-test (self place)
  (:documentation
   "Push `test' SELF to the value of slot ':tests' in `test-suite' object PLACE."))

(defgeneric pop-test (self)
  (:documentation
   "Pop the first `test' from the slot-value of ':tests' in `test-suite' object SELF."))

(defgeneric push-result (self place)
  (:documentation
   "Push object SELF to the value of slot ':results' in object PLACE."))

(defgeneric pop-result (self)
  (:documentation
   "Pop the first `test-result' from the slot-value of ':tests' from object SELF."))

(defgeneric push-fixture (self place)
  (:documentation
   "Push object SELF to the value of slot ':results' in object PLACE."))

(defgeneric delete-test (self &key &allow-other-keys)
  (:documentation "Delete `test' object specified by `test-object' SELF and optional keys."))

(defgeneric find-test (self name &key &allow-other-keys)
  (:documentation "Find `test' object specified by name and optional keys."))

(defgeneric do-test (self &optional context)
  (:documentation "Run test SELF, printing results to *standard-output*. The second
argument is an optional fixture.

SELF can also be a `test-suite', in which case the TESTS slot is
queried for the value of TEST. If TEST is not provided, pops the car
from TESTS."))

(defgeneric do-suite (self &key &allow-other-keys)
  (:documentation
   "Perform actions on `test-suite' object SELF with optional keys."))

(defgeneric make-fixture (kind &rest args &key &allow-other-keys)
  (:documentation
   "Make a FIXTURE object with optional init ARGS."))
