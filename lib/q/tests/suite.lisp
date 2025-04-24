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
  (let ((tbl (make-hash-table :test 'equal)))
    (setf (gethash "FOO" tbl) (make-df))
    (with-sql (expr "SELECT * FROM FOO" :optimize t)
      (is (typep expr 'sql-select))
      (is (gethash "FOO" tbl))
      (istype 'data-frame (make-sql-df expr tbl)))))

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
  (color 1 red a)
  (color 1 red b) 
  (color 2 blue a)
  (color 2 blue b) 
  (color 3 green a)
  (color 3 green b) 
  (color 4 yellow a)
  (color 4 blue b) 
  (color 5 blue a)
  (color 5 green b))

#| SL
Exercise 2.9. Translate to clausal logic:
(a) every mouse has a tail;
(b) somebody loves everybody;
(c) every two numbers have a maximum.
|#
(deftest dql-clausal-simple (:skip t))

;; ref: https://en.wikipedia.org/wiki/Zebra_Puzzle

;; ref: https://franz.com/support/documentation/11.0/prolog.html
(deftest dql-zebra (:skip t)
  "A solution for the Zebra problem using DQL."
  (<-- (nextto ?x ?y ?list) (iright ?x ?y ?list))
  (<-  (nextto ?x ?y ?list) (iright ?y ?x ?list))
  (<-- (iright ?left ?right (?left ?right . ?rest)))
  (<-  (iright ?left ?right (?x . ?rest))
       (iright ?left ?right ?rest))
  (<-- (zebra ?h ?w ?z)
       ;; Each house is of the form:
       ;; (house nationality pet cigarette drink house-color)
       (= ?h ((house norwegian ? ? ? ?)   ;1,10
              ?
              (house ? ? ? milk ?) ? ?))  ; 9
       (member (house englishman ? ? ? red) ?h) ; 2
       (member (house spaniard dog ? ? ?) ?h) ; 3
       (member (house ? ? ? coffee green) ?h) ; 4
       (member (house ukrainian ? ? tea ?) ?h) ; 5
       (iright (house ? ? ? ? ivory)      ; 6
               (house ? ? ? ? green) ?h)
       (member (house ? snails winston ? ?) ?h) ; 7
       (member (house ? ? kools ? yellow) ?h) ; 8
       (nextto (house ? ? chesterfield ? ?) ;11
               (house ? fox ? ? ?) ?h)
       (nextto (house ? ? kools ? ?)      ;12
               (house ? horse ? ? ?) ?h)
       (member (house ? ? luckystrike oj ?) ?h) ;13
       (member (house japanese ? parliaments ? ?) ?h) ;14
       (nextto (house norwegian ? ? ? ?)  ;15
               (house ? ? ? ? blue) ?h)
       (member (house ?w ? ? water ?) ?h) ;Q1
       (member (house ?z zebra ? ? ?) ?h)) ;Q2
  ;; execute the query
  (?- (zebra ?houses ?water-drinker ?zebra-owner))
  ;; It is believed that solving zebra a
  ;; single time requires 12825 inferences.
  )
