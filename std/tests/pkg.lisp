;;; tests.lisp --- std system tests

;;; Commentary:

;;

;;; Code:
(in-package :std-int)
(defpkg :std/tests
  (:use :cl :std :rt :sb-thread :sb-alien))
(in-package :std/tests)
(defsuite :std)
(in-suite :std)
(in-readtable :std)
;; prevent threadlocks
(setf sb-unix::*on-dangerous-wait* :error)

(deftest readtables ()
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
  )

(deftest sym ()
  "Test standard symbol utils"
  ;; gensyms
  (is (not (equalp (make-gensym 'a) (make-gensym 'a))))
  (is (eq 'std/tests::foo (format-symbol :std/tests "~A" 'foo)))
  (is (eq (make-keyword 'fizz) :fizz))
  (iseql 'foo (ensure-symbol "FOO"))
  (iseql 'abc (symb 'a 'b 'c))
  (iseq :function (fboundp! 'car))
  (iseq :special (vboundp! '*standard-output*))
  (alias-function foo-car car)
  (is= (foo-car (list 1 2)) 1))

(deftest string ()
  "Test standard string utils"
  (is (typep "test" 'string-designator))
  (is (typep 'test 'string-designator))
  (is (typep #\C 'string-designator))
  (is (not (typep 0 'string-designator)))
  (isequal "abc" (concatenate 'string (char-range #\a #\c)))
  (is (ascii-ichar= #\A #\a))
  (is (ascii-istring= "foObAr" "foobar"))
  (let ((str "abc"))
    (isequal (nconcat str "def" str) "abcdefabc")
    (is
     (string-case (str)
       ("def")
       ("abc" t)))))

(deftest list ()
  "Test standard list utils"
  ;; same object - a literal
  (is (eq (ensure-car '(0)) (ensure-car 0)))
  (is (eq (ensure-car '(nil)) (ensure-car nil)))
  ;; different objects
  (is (not (eq (ensure-cons 0) (ensure-cons 0))))
  (is (equal (ensure-cons 0) (ensure-cons 0))))

(deferror testing-error (std-error) nil (:auto t) (:documentation "testing"))

(deftest err ()
  "Test standard error handlers"
  (signals testing-error (testing-error)))

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
  (is= 2 (let ((x 1)) (pandoric-eval (x) '(+ 1 x))))
  (is= 2 (let ((x 1)) (pandoric-eval (x) '(incf x))))
  (let ((p
          (let ((a 0))
            (let ((b 1))
              (plambda (n) (a b)
                       (incf a n)
                       (setq b (* b n)))))))
    (with-pandoric (a b) p
      (is (= 0 (funcall p 0)))
      (setf b 4)
      (is= 16 (funcall p 4) b)
      (is= 4 a)
      (is= 16 (funcall p 1) b)
      (is= 5 a))))

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
