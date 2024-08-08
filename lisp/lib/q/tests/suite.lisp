;;; tests.lisp --- Q Tests

;; 

;;; Code:
(in-package :q/tests)

(defsuite :q)

(in-suite :q)

(deftest sanity ()
  (is (make-instance 'query-engine
        :parser (make-instance 'query-parser)
        :optimizer (make-instance 'sql-optimizer)
        :sources nil)))

(deftest sql-select ()
  (setf (gethash "FOO" tbl) (make-df nil))
  (with-sql (expr "SELECT I FROM FOO")
    (is (typep expr 'sql-select))
    (let ((tbl (make-hash-table :test 'equalp)))
      (is (gethash "FOO" tbl))
      (make-sql-data-frame expr tbl))))

(deftest sql-math ()
  (with-sql (expr "1 + 2 * 3")
    (is (typep expr 'sql-math-expression))
    (is (typep (rhs expr) 'sql-math-expression))
    (is (typep (lhs expr) 'sql-number))))

;; https://www.cpp.edu/~jrfisher/www/prolog_tutorial/2_1.html
(deftest dql (:skip t)
  (adjacent 1 2)
  (adjacent 2 1) 
  (adjacent 1 3)
  (adjacent 3 1) 
  (adjacent 1 4)
  (adjacent 4 1) 
  (adjacent 1 5)
  (adjacent 5 1) 
  (adjacent 2 3)
  (adjacent 3 2) 
  (adjacent 2 4)
  (adjacent 4 2) 
  (adjacent 3 4)
  (adjacent 4 3) 
  (adjacent 4 5)
  (adjacent 5 4) 
  (color 1 red a)    (color 1 red b) 
  (color 2 blue a)   (color 2 blue b) 
  (color 3 green a)  (color 3 green b) 
  (color 4 yellow a) (color 4 blue b) 
  (color 5 blue a)   (color 5 green b)

  (:- (conflict ?coloring)
      (adjacent ?x ?y)  
      (color ?x ?color ?coloring)  
      (color ?y ?color ?coloring))


  (:- (conflict ?r1 ?r2 ?coloring)
      (adjacent ?r1 ?r2)  
      (color ?r1 ?color ?coloring)  
      (color ?r2 ?color ?coloring))


  ;; there are several infix operators.
  ;; :- , >, <, -> etc.
  ;; let's mark variables with ? prefix.
  ;; 

  (:- main
      (forall (conflict ?coloring)
              (writeln (conflict ?coloring)))
      (forall (conflict ?r1 ?r2 ?coloring)
              (writeln (conflict ?r1 ?r2 ?coloring)))
      (forall (conflict ?r1 ?r2 ?coloring)
              (and (print-sexp (conflict ?r1 ?r2 ?coloring))
                   nl))
      halt)

  (:- (initialization main)))
