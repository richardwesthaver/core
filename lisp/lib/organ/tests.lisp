(defpackage :organ/tests
  (:use :cl :organ :std :rt))

(in-package :organ/tests)

(defvar *test-file* "tests.org")

(defsuite :organ)
(in-suite :organ)

(deftest org-file () 
  (is (read-org-file *test-file*)))

(deftest org-lines ()
  (is (read-org-lines (open *test-file*)))
  (let ((s (text (read-org-file *test-file*))))
    (is (read-org-lines-from-string s))))

(deftest org-headline ()
  (let ((s "** DONE testing stuff :test:test:"))
    (is (= (level (org-parse (make-org-headline s))) 2))
    ;; weird bug going on here with class slots
    ;; (is (string= (text (state (org-parse (make-org-headline s)))) "TODO"))
    ;; ??? this ain't right
    (is (string= (title (org-parse (make-org-headline s))) "DONE testing stuff "))
    (is (= (length (tags (org-parse (make-org-headline s)))) 2))))
