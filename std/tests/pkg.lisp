;;; tests.lisp --- std system tests

;;; Commentary:

;;

;;; Code:
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
  (is (equal (funcall [#'values] 1 2 3) (values 1 2 3))) #|compose.4|#)

(alias-function foo-car car)

(deftest sym ()
  "Test standard symbol utils"
  ;; gensyms
  (is (not (equalp (make-gensym 'a) (make-gensym 'a))))
  (is (eq 'std/tests::foo (format-symbol :std/tests "~A" 'foo)))
  (is (eq (make-keyword 'fizz) :fizz))
  (iseql (intern "FOO") (ensure-symbol "FOO"))
  (iseql (intern "ABC") (symb 'a 'b 'c))
  (iseq :function (fboundp! 'car))
  (iseq :special (vboundp! '*standard-output*))
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

;; STD-* prints all arguments
(deferror testing-error (std-error) nil (:auto t) (:documentation "testing"))

;; simple-* prints a control string and format args
(defwarning testing-warning (simple-warning) () (:auto t))

(deftest conditions ()
  "Test standard error handlers"
  (signals testing-error (testing-error "foo"))
  (signals testing-warning (testing-warning "foo:~A" 'bar))
  (istype 'wrapped-error (wrap-error (make-condition 'testing-error)))
  (signals simple-error (std/condition:nyi!)))

(deftest fmt ()
  "Test standard formatters"
  (is (string= (format nil "| 1 | 2 | 3 |~%") (with-output-to-string (s) (fmt-row s '(1 2 3)))))
  (is (string= 
       ;; note the read-time-eval..
       #.(with-output-to-string (s) (fmt-tree s '(foobar (:a) (:b) (c) (d)) :layout :down))
       #"FOOBAR
 ├─ :A
 ├─ :B
 ├─  C
 ╰─  D
"#))
  ;; with plist option
  (is (string= 
       #.(with-output-to-string (s) 
           (fmt-tree s '(sk-project :name "foobar" :path "/a/b/c.asd" :vc :hg) :layout :down :plist t))
       #"SK-PROJECT
 ├─ :NAME
 │   ╰─ "foobar"
 ├─ :PATH
 │   ╰─ "/a/b/c.asd"
 ╰─ :VC
     ╰─ :HG
"#))
  (is (string= "| A | B | C |
" (with-output-to-string (s) (fmt-row s '(a b c))))))

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

(deftest comp ()
  (inspect-ir '(lambda (a b c) a b c) (lambda (x) (istype 'sb-c:component x)))
  (isnt (zerop (length (asm-search "MOV" #'car))))
  (istype 'vector (std/comp::%asm '())))

(deftest serde ()
  (is= (type-id t) 3117)
  (is= (simple-type-id t) 12)
  (is= (simple-type-id #\c)
       (ldb (byte 8 8) (type-id #\c)))
  (isnt (= (type-id (make-array 42 :element-type 'character))
           (type-id (make-array 0 :element-type 'base-char)))))
