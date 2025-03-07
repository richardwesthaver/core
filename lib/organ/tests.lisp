(defpackage :organ/tests
  (:use :cl :organ :std :rt :rt/fuzz)
  (:export *test-org-file*))

(in-package :organ/tests)
(in-readtable :std)

(defclass org-fuzzer (fuzzer) ()
  (:default-initargs
   :state nil
   :generator 
   (lambda (state)
     (org-create 
      (or state
          (keywordicate
           (random-elt (append org-element-objects org-element-types))))))))

(defparameter *test-org-heading* 
  #"* TODO [#A] header1                       :tag1:tag2:
:PROPERTIES:
:ID: 1234
:CUSTOM_ID: 5678
:END:
"#)

(defparameter *test-org-section*
  "Paragraph with /italics/ *bold* =verbatim= ~code~ _underline_ +strike-through+.

#+begin_src lisp
(print \"hello world\")
#+end_src")

(defparameter *test-org-lines*
  "Plain text.
/Italics/
*bold*
=verbatim=
~code~
_underline_
+strike-through+")

(defsuite :organ)
(in-suite :organ)

;;; Objects
(deftest org-markup ()
  "Test org markup in a paragraph."
  (let ((lines (read-org-lines-from-string *test-org-lines*)))
    (is (org-parse :plain-text (aref lines 0)))
    (is (org-parse :italic (aref lines 1)))
    (is (org-parse :bold (aref lines 2)))
    (is (org-parse :verbatim (aref lines 3)))
    (is (org-parse :code (aref lines 4)))
    (is (org-parse :underline (aref lines 5)))
    (is (org-parse :strike-through (aref lines 6))))
  ;; should return vector of ORG-OBJECTs
  (is (typep (org-contents (org-parse :paragraph *test-org-lines*)) 'vector)))

(deftest org-minimal ())

(deftest org-standard ())

(defun headline-ok (hl)
  (is
   (and
    (> (organ::hl-stars hl) 0)
    (organ::hl-kw hl)
    (organ::hl-priority hl)
    (organ::hl-title hl)
    (> (length (organ::hl-tags hl)) 0))))

;;; Elements
(deftest org-headline () (is (headline-ok (org-parse :headline "** DONE [#A] testing stuff :foo:bar:"))))

;;; API
(deftest org-heading ()
  (is (headline-ok (org-headline (org-parse :heading *test-org-heading*)))))

(deftest org-section ())

(deftest org-document ())

(deftest org-lines ()
  (is (vectorp (read-org-lines-from-string *test-org-heading*))))
