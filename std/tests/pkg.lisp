;;; tests.lisp --- std system tests

;;; Commentary:

;; TODO: fix false positives when using (eval-test)

;;; Code:
(in-package :std-int)
(defpkg :std/tests
  (:use :cl :std :rt :sb-thread :sb-alien))
(in-package :std/tests)
(defsuite :std)
(in-suite :std)
(in-readtable :std)
;; prevent threadlocks
;; (setf sb-unix::*on-dangerous-wait* :error)

;; TODO 2024-05-14: fix compilation order of std/fu vs std/readtables
(deftest readtables (:skip nil)
  "Test :std readtable"
  (is (typep #`(,a1 ,a1 ',a1 ,@a1) 'function))
  (is (string= #"test "foo" "# "test \"foo\" "))
  ;; from curry-compose-reader-macros test suite
  (is (equal (funcall {list 1} 2) '(1 2))) ;; curry.1
  (is (equal (mapcar {+ 1} '(1 2 3 4)) '(2 3 4 5))) ;; curry.2
  (is (equal (funcall {1 list 1} 2) '(1 2))) ;; curry.fixed-arity
  (is (equal (funcall {2 list _ 2} 3 4) '(3 4 2))) ;; curry.fixed-arity.2
  (signals error
    (let ((f {1 list 1}))
      (progn (funcall f) nil))) ;; curry.fixed-arity.1
  (signals error
    (locally (declare (optimize safety))
      (let ((f {1 list 1}))
        (progn (funcall f 'a 'b) nil)))) ;; curry.fixed-arity-error.2
  (is (equal (funcall {list _ 1} 2) '(2 1))) ;; rcurry.1
  (is (equal (mapcar {- _ 1} '(1 2 3 4)) '(0 1 2 3))) ;; rcurry.2
  (is (equal (funcall [{* 3} #'1+] 1) 6)) ;; compose.1
  (is (equal (funcall ['1+ '1+] 1) 3)) ;; compose.2
  (is (equal (funcall [#'1+] 1) 2)) ;; compose.3
  (is (equal (funcall [#'values] 1 2 3) (values 1 2 3))) ;; compose.4
  (is (equal (funcall «list {* 2} {* 3}» 4) '(8 12))) ;; join.1
  (is (equal (mapcar «and {< 2} 'evenp (constantly t)» '(1 2 3 4)) (list nil nil nil t))) ;; join.2
  ;; typecase-bracket
  (is (equal (mapcar ‹typecase (number #'1+) (string :str)› '(1 "this" 2 "that")) '(2 :str 3 :str)))
  ;; cond-bracket
  (is (equal (mapcar ‹cond (#'evenp {+ 100}) (#'oddp {+ 200})› '(1 2 3 4)) '(201 102 203 104)))
  ;; if-bracket
  (is (equal (mapcar ‹if #'evenp {list :a} {list :b}› '(1 2 3 4))
             '((:b 1) (:a 2) (:b 3) (:a 4))))
  ;; when-bracket
  (is (equal (mapcar ‹when 'evenp {+ 4}› '(1 2 3 4)) (list nil 6 nil 8)))
  ;; unless-bracket
  (is (equal (mapcar ‹unless 'evenp {+ 4}› '(1 2 3 4)) (list 5 nil 7 nil))))

(deftest sym ()
  "Test standard symbol utils"
  ;; gensyms
  (is (not (equalp (make-gensym 'a) (make-gensym 'a))))
  (is (eq 'std/tests::foo (format-symbol :std/tests "~A" 'foo)))
  (is (eq (make-keyword 'fizz) :fizz)))

;;;; TODO
(deftest string ()
  "Test standard string utils"
  (is (typep "test" 'string-designator))
  (is (typep 'test 'string-designator))
  (is (typep #\C 'string-designator))
  (is (not (typep 0 'string-designator))))

(deftest list ()
  "Test standard list utils"
  ;; same object - a literal
  (is (eq (ensure-car '(0)) (ensure-car 0)))
  (is (eq (ensure-car '(nil)) (ensure-car nil)))
  ;; different objects
  (is (not (eq (ensure-cons 0) (ensure-cons 0))))
  (is (equal (ensure-cons 0) (ensure-cons 0))))

(deftest err ()
  "Test standard error handlers"
  (is (eql 'testing-err (deferror testing-err (std-error) nil (:auto t) (:documentation "testing")))))

(deftest fmt ()
  "Test standard formatters"
  (is (string= (format nil "| 1 | 2 | 3 |~%") (fmt-row '(1 2 3))))
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
  "Test standard anaphoric macros"
  (is (= 8 
	 (aif (+ 2 2)
	      (+ it it))))
  (is (= 42 (awhen 42 it)))
  (is (= 3 (acond ((1+ 1) (1+ it)))))
  (loop for x in '(1 2 3)
        for y in (funcall (alet* ((a 1) (b 2) (c 3))
                                 (lambda () (mapc #'1+ (list a b c)))))
        collect (is (= x y))))

(deftest pan ()
  "Test standard pandoric macros"
  (let ((p
	  (plambda (a) (b c)
		   (if (not a)
		       (setq b 0
			     c 0)
		       (progn (incf b a) (incf c a))))))
    (with-pandoric (b c) p
      (is (= 0 (funcall p nil)))
      (is (= 1 (funcall p 1)))
      (is (= 11 (funcall p 10)))
      (is (= 0 (funcall p nil)))
      )))

(deftest alien ()
  "Test standard alien utils"
  (is (= 0 (foreign-int-to-integer 0 4)))
  (is (= 1 (bool-to-foreign-int t)))
  (istype 
   '(alien (* (unsigned 8))) 
   (write-alien :octet-vector (std:make-octets 10) (make-alien unsigned-char 10))))

(deftest curry ()
  "Test curry functions from Alexandria, found in std/fu.
These tests are copied directly from the Alexandria test suite."
  ;; curry.1
  (let ((curried (curry '+ 3)))
    (is (= (funcall curried 1 5) 9)))
  ;; curry.2
  (let ((curried (locally (declare (notinline curry))
                   (curry '* 2 3))))
    (is (= (funcall curried 7) 42)))
  ;; curry.3
  (let ((curried-form (funcall (compiler-macro-function 'curry)
                               '(curry '/ 8)
                               nil)))
    (let ((fun (funcall (compile nil `(lambda () ,curried-form)))))
      (is (= (funcall fun 2) 4)))) ;; maybe fails?
  ;; curry.4
  (let* ((x 1)
         (curried (curry (progn
                           (incf x)
                           (lambda (y z) (* x y z)))
                         3)))
    (is (equal (list (funcall curried 7)
                     (funcall curried 7)
                     x)
               '(42 42 2))))
  ;; rcurry.1
  (let ((r (rcurry '/ 2)))
    (is (= (funcall r 8) 4)))
  ;; rcurry.2
  (let* ((x 1)
         (curried (rcurry (progn
                            (incf x)
                            (lambda (y z) (* x y z)))
                          3)))
    (is (equalp 
         (list (funcall curried 7) ;; 42
               (funcall curried 7) ;; 42
               x) ;; 2
         '(42 42 2)))))

(define-bitfield testbits
  (a boolean)
  (b (signed-byte 2))
  (c (unsigned-byte 3) :initform 1)
  (d (integer -100 100))
  (e (member foo bar baz)))

(deftest bits ()
  (let ((bits (make-testbits)))
    (is (not (testbits-a bits)))
    (is (= 0 (testbits-b bits)))
    (is (= 1 (testbits-c bits)))
    (is (= -100 (testbits-d bits)))
    (is (eql 'foo (testbits-e bits)))))
