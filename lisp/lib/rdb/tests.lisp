(defpackage :rdb/tests
  (:use :std/base :rt :rdb))
(in-package :rdb/tests)
(defsuite :rdb)
(in-suite :rdb)

(deftest with-db ())
(deftest with-iter ())
