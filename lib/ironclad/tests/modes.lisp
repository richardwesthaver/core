;;; tests/modes.lisp --- sanity checks for mode reversibility

;;; Code:
(in-package :ironclad/tests)

(deftest :modes.cbc ()
  (run-test-vector-file :cbc *mode-tests*)
  t)

(deftest :modes.cbc.padding ()
  (run-test-vector-file :cbc *mode-padding-tests*)
  t)

(deftest :modes.cfb ()
  (run-test-vector-file :cfb *mode-tests*)
  t)

(deftest :modes.cfb8 ()
  (run-test-vector-file :cfb8 *mode-tests*)
  t)

(deftest :modes.ofb ()
  (run-test-vector-file :ofb *mode-tests*)
  t)

(deftest :modes.ctr ()
  (run-test-vector-file :ctr *mode-tests*)
  t)
