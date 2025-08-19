;;; cfg.lisp --- Test Configuration

;; RT User Configuration

;;; Code:
(in-package :rt)

(defconfig test-config () ()
  (:documentation "User configuration of the RT testing environment. Calling BUILD on this object
may affect the following special variables:
*TEST-POLICY*
*COMPILE-TESTS*
*CATCH-TEST-ERRORS*
*TEST-ON-DEF*
*TEST-SUFFIX*
*TEST-SUITE*
*TEST-INPUT*
*TEST-OUTPUT*
*TMP*
*COVERAGE-DIRECTORY*"))

(defmethod make-config ((self (eql :test)) &rest args)
  (apply 'make-instance 'test-config args))
