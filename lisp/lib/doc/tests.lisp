;;; doc/tests.lisp --- DOC tests

;;

;;; Code:
(defpackage :doc/tests
  (:use :cl :rt :doc))

(in-package :doc/tests)

(defsuite :doc)
(in-suite :doc)

(deftest doc-symbol ()
  (is (typep (symbol-documentation 'car) 'symbol-documentation)))

(deftest doc-package ()
  (is (typep (package-documentation) 'package-documentation)))

(deftest doc-system ()
  (is (typep (system-documentation :std) 'system-documentation)))

(deftest doc-file ()
  (let ((file (or *compile-file-pathname* (asdf:system-relative-pathname :doc "tests.lisp"))))
    (is (typep (file-documentation file) 'file-documentation))))

(deftest doc-dist ()
  (is (typep (dist-documentation :quicklisp) 'dist-documentation)))

(deftest image-documentation ()
  (is t))

(deftest doc-db ()
  (rocksdb:load-rocksdb))
