(defpackage :nlp/tests
  (:use :cl :std :rt :nlp))

(in-package :nlp/tests)

(defsuite :nlp)
(in-suite :nlp)

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
    (is (= 3 (nlp/doc:term-count
              doc "test")))
    (is (= 1.0 (nlp/doc:term-frequency
                doc "test")))))

(deftest textrank ()
  (istype 'list 
          (summarize-text 
           "This is a test which will be summarized by the 'SUMMARIZE-TEXT' function. Yada yada. Test 1 2 3.")))

(deftest dbscan ()
  (dbscan (extract-sections "This is a test which will be summarized by the 'SUMMARIZE-TEXT' function. Yada yada. Test 1 2 3.")))
