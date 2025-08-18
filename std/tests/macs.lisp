;;; macs.lisp --- Macro tests

;; 

;;; Code:
(in-package :std/tests)
(in-suite :std)
(deftest collecting ()
  (is= 10 (length (collecting (loop for i below 10 do (collect 0))))))
    
(deftest nested-loop ()
  (is= 2800 (reduce #'+ (collecting (nested-loop (i j) '(10 20) (collect (+ i j)))))))
    
(deftest units ()
  (istype 'distance-designator :km)
  (istype 'distance-designator :light-year))

(deftest defvars ()
  (defvar-unbound frobz)
  (isnt (boundp 'frobz)))

(deftest ifret ()
  (is (ifret t (error "ifret failed")))
  (iseq :abc (ifret nil :abc)))


(define-constant %%frob1$$ 0 :documentation "a dummy constant")

(deftest define-constant ()
  (is (constantp %%frob1$$)))

(deftest switch ()
  (is
   (switch (123 :test 'typep)
     ('float nil)
     ('integer t)
     (t nil))))

(deftest xor ()
  (multiple-value-bind (x y) (xor nil t nil)
    (is x)
    (is y))
  (multiple-value-bind (x y) (xor nil nil)
    (isnt x)
    (is y))
  (multiple-value-bind (x y) (xor t t t t t)
    (isnt x)
    (isnt y)))

(deftest lets ()
  (lety ((foo 0 :type fixnum)
         (arr (make-octets 3) :type octet-vector))
   (istype 'fixnum foo)
   (istype 'octet-vector arr)))

(deftest defs ()
  (defityped %%froyo ((self string)) simple-string (declare (ignore self)) "bar")
  (deftyped* %%froyo1 ((self string)) self)
  (is (string= "bar" (%%froyo "foo")))
  (is (string= "baz" (%%froyo1 "baz"))))

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
