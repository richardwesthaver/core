;;; tests.lisp --- macs system tests

;;; Commentary:

;; TODO: fix false positives when using (eval-test)

;;; Code:
(uiop:define-package :std/tests
  (:use :cl :std :rt))
   
(in-package :std/tests)

(in-readtable :std)

;;; READTABLES
(defsuite :named-readtables)
(in-suite :named-readtables)
(deftest readtables ()
  "Test :std readtable without cl-ppcre"
  (is (typep #`(,a1 ,a1 ',a1 ,@a1) 'function))
  (is (string= #"test "foo" "# "test \"foo\" "))
  (is (string= #$test "1 2 3"$# "test \"1 2 3\"")))

;;; STD
(defsuite :std)
(in-suite :std)

(deftest sym ()
  "Test STD.SYM"
  ;; gensyms
  (is (not (equalp (make-gensym 'a) (make-gensym 'a))))
  (is (eq 'std/tests::foo (format-symbol :std/tests "~A" 'foo)))
  (is (eq (make-keyword 'fizz) :fizz)))

;;;; TODO
(deftest str ()
  "Test STD.STR"
  (is (typep "test" 'string-designator))
  (is (typep 'test 'string-designator))
  (is (typep #\C 'string-designator))
  (is (not (typep 0 'string-designator))))

(deftest list ()
  "Test STD.LIST"
  ;; same object - a literal
  (is (eq (ensure-car '(0)) (ensure-car 0)))
  (is (eq (ensure-car '(nil)) (ensure-car nil)))
  ;; different objects
  (is (not (eq (ensure-cons 0) (ensure-cons 0))))
  (is (equal (ensure-cons 0) (ensure-cons 0))))

(deftest cond ()
  "Test STD.COND")

(deftest thread ()
  "Test STD.THREAD"
  (is (stringp (print-thread-info nil))))

(deftest fmt ()
  "Test STD.FMT"
  (is (string= (format nil "| 1 | 2 | 3 |~%") (fmt-row '(1 2 3))))
  (is (string= (fmt-sxhash (sxhash t)) (fmt-sxhash (sxhash t))))
  (is (string= 
       ;; note the read-time-eval..
       #.(fmt-tree nil '(foobar (:a) (:b) (c) (d)) :layout :down)
       #"FOOBAR
 ├─ :A
 ├─ :B
 ├─  C
 ╰─  D
"#))
;; with plist option
  (is (string= 
       #.(std:fmt-tree nil '(sk-project :name "foobar" :path "/a/b/c.asd" :vc :hg) :layout :down :plist t)
       #"SK-PROJECT
 ├─ :NAME
 │   ╰─ "foobar"
 ├─ :PATH
 │   ╰─ "/a/b/c.asd"
 ╰─ :VC
     ╰─ :HG
"#)))

(deftest ana ()
  "Test STD.ANA"
  (is (= 8 
	 (aif (+ 2 2)
	      (+ it it)))))

(deftest pan ()
  "Test STD.PAN"
  (let ((p
	  (plambda (a) (b c)
		   (if (not a)
		       (setq b 0
			     c 0)
		       (progn (incf b a) (incf c a))))))
    (with-pandoric (b c) p
      (is (= 0 (funcall p nil)))
      (is (= 1 (funcall p 1)))
      (is (= 1 b c)))))

(deftest alien ()
  "Test alien utils."
  (is (= 0 (foreign-int-to-integer 0 4)))
  (is (= 1 (bool-to-foreign-int t))))
