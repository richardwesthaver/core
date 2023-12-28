(defpackage :organ/tests
  (:use :cl :organ :std :rt)
  (:export *test-org-file*))

(in-package :organ/tests)

(defparameter *test-org-heading* 
  "* header1                                                         :tag1:tag2:
:PROPERTIES:
:ID: 1234
:CUSTOM_ID: 5678
:END:
")

(defparameter *test-org-paragraph*
  "Plain text.
/Italics/
*bold*
=verbatim=
~code~
_underline_
+strike-through+")

(defsuite :organ)
(in-suite :organ)

(deftest org-markup ()
  "Test org markup in a paragraph."
  (let ((lines (read-org-lines-from-string *test-org-paragraph*)))
    (is (org-parse :plain-text (aref lines 0)))
    (is (org-parse :italic (aref lines 1)))
    (is (org-parse :bold (aref lines 2)))
    (is (org-parse :verbatim (aref lines 3)))
    (is (org-parse :code (aref lines 4)))
    (is (org-parse :underline (aref lines 5)))
    (is (org-parse :strike-through (aref lines 6))))
  ;; should return vector of ORG-OBJECTs
  (is (typep (org-contents (org-parse :paragraph *test-org-paragraph*)) 'vector)))

(deftest org-headline ()
  (let* ((s "** DONE [#A] testing stuff :foo:bar:")
         (hl (org-parse :headline s)))
    (is (= (organ::hl-stars hl) 2))
    (is (organ::hl-kw hl))
    (is (organ::hl-priority hl))
    (is (string= (organ::hl-title hl) "testing stuff"))
    (is (= (length (organ::hl-tags hl)) 2))))

(deftest org-lines ()
  (is (vectorp (read-org-lines-from-string *test-org-heading*))))

