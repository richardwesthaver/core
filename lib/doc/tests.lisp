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

(eval-always
  (defmacro is-doc-typep (type arg)
    `(is (typep ,arg ',type))))

(deftest doc-symbol ()
  (is-doc-typep symbol-documentation (symbol-documentation 'car)))

(deftest doc-package ()
  (is-doc-typep package-documentation (package-documentation)))

(deftest doc-system ()
  (is-doc-typep system-documentation (system-documentation :std)))

(deftest doc-file ()
  (let ((file (or *compile-file-pathname* (system-relative-pathname :doc "tests.lisp"))))
    (is-doc-typep file-documentation (file-documentation file))))

;; TODO 2025-08-18: 
(deftest doc-db (:skip :todo)
  (db:load-database-backend :rdb))

;;; NLP
(defvar *test-docs* (make-instance 'document-collection))

(deftest tokenize ()
  (is (= 3 (length (word-tokenize "foo bar baz"))))
  (is (= 2 (length (sentence-tokenize "This is the first second. Now the second")))))

(deftest sections ()
  (is (typep (extract-sections "Testing 1 2 3") 'document-collection)))

(deftest porter-stem ()
  (is (string= (stem "hacking") "hack")))

(deftest docs ()
  (let ((doc (make-instance 'document :string-contents "test test test")))
    (is (= 3 (nlp:term-count
              doc "test")))
    (is (= 1.0 (nlp:term-frequency
                doc "test")))))

(deftest textrank ()
  (istype 'list 
          (summarize-text 
           "This is a test which will be summarized by the 'SUMMARIZE-TEXT' function. Yada yada. Test 1 2 3.")))

(deftest dbscan ()
  (dbscan (extract-sections "This is a test which will be summarized by the 'SUMMARIZE-TEXT' function. Yada yada. Test 1 2 3.")))

(deftest fuzzy ()
  (isequal '("foobar" "barfoo") (fuzzy:fuzzy-match "foo bar" '("asdfasdoo" "barfoo" "foobar"))))
