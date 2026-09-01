;;; proto.lisp --- Test Protocols

;; 

;;; Code:
(in-package :rt)

(defgeneric eval-test (self)
  (:documentation "Eval a TEST."))

(defgeneric funcall-test (self &key &allow-other-keys)
  (:documentation "Funcall a TEST."))

(defgeneric compile-test (self &key &allow-other-keys)
  (:documentation "Compile a TEST."))

(defgeneric push-test (self place)
  (:documentation
   "Push SELF to the value of slot ':tests' in PLACE."))

(defgeneric pop-test (self)
  (:documentation
   "Pop the first TEST from the slot-value of ':tests' in TEST-SUITE object SELF."))

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
  (:documentation "Delete  TEST-OBJECT specified by SELF."))

(defgeneric test (self name &key &allow-other-keys)
  (:documentation "Find TEST-OBJECT specified by name."))

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
