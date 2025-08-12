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
  (istypep :km 'distance-designator)
  (istypep :light-year 'distance-designator))

(deftest defvars ()
  (defvar-unbound frobz)
  (isnt (boundp 'frobz)))

(deftest ifret ()
  (is (ifret t (error)))
  (iseq :abc (ifret nil :abc)))

(deftest define-constant ()
  (define-constant %%frob1$$ 0 :documentation "a dummy constant")
  (is (constantp %%frob1$$)))

;; TODO 2025-08-11: 
(deftest xor ())

(deftest switch ())

(deftest lets ())

(deftest classy ())
