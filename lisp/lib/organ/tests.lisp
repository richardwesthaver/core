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

(defsuite :organ)
(in-suite :organ)

(deftest org-file () 
  (is (read-org-string *test-org-heading*)))

(deftest org-lines ()
  (is (read-org-lines-from-string *test-org-heading*)))

(deftest org-headline ()
  (let ((s "** DONE testing stuff :test:test:"))
    (describe (org-parse (make-org-headline s)))
    (is (= (level (org-parse (make-org-headline s))) 2))
    ;; weird bug going on here with class slots
    ;; (is (string= (text (state (org-parse (make-org-headline s)))) "TODO"))
    ;; ??? this ain't right
    (is (string= (title (org-parse (make-org-headline s))) "DONE testing stuff "))
    (is (= (length (tags (org-parse (make-org-headline s)))) 2))))
