;;; doc/tests.lisp --- DOC tests

;;

;;; Code:
(defpackage :doc/tests
  (:use :cl :rt :doc :std))

(in-package :doc/tests)

(defsuite :doc)
(in-suite :doc)
(in-readtable :std)

(defvar *good-header* #";;; foo/bar.lisp --- a dummy lisp description

;; a dummy lisp summary

;;; Code:
"#)

(defvar *bad-header* #";; not a valid header -- :'C

;;
'boop"#)

(defvar *good-heading* #";;; Foobar:
"#)

(defvar *bad-heading* #";;;Foobar:"#)

(defmacro is-doc-typep (type arg)
  `(is (typep ,arg ',(symbolicate type "-DOCUMENTATION"))))

(deftest doc-symbol ()
  (is-doc-typep :symbol (symbol-documentation 'car)))


(deftest doc-package ()
  (is-doc-typep :package (package-documentation)))

(deftest doc-system ()
  (is-doc-typep :system (system-documentation :std)))

(deftest doc-file ()
  (let ((file (or *compile-file-pathname* (asdf:system-relative-pathname :doc "tests.lisp"))))
    (is-doc-typep :file (file-documentation file))))

(deftest doc-dist ()
  (is-doc-typep :dist (dist-documentation :quicklisp)))

(deftest image-documentation ()
  (is t))

(deftest doc-db ()
  (rocksdb:load-rocksdb))
