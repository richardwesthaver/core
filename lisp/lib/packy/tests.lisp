(defpackage :packy/tests
  (:use :cl :rt :packy :rdb :db))

(in-package :packy/tests)
(defsuite :packy)
(in-suite :packy)
(load-database-backend :packy)

(deftest packy-db ()
  (with-db (db :db (make-db :packy) :open t :close t)
    (is (db-open-p db))))

(deftest packy-objects ())
