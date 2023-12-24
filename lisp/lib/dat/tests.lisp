(defpackage :dat/tests
  (:use :cl :std :rt :dat))

(in-package :dat/tests)

(defsuite :dat)
(in-suite :dat)

(deftest csv ())

(deftest json ()
  (let ((str (format nil "[~s,2,true,null]" "Hello, world!"))
        (obj (list "Hello, world!" 2 t nil)))
    (multiple-value-bind (res pos) (json-decode str)
      (is (equal obj res))
      (is (= pos 29)))
    (is (equal str (with-output-to-string (s) (json-encode obj s)))))
  (let ((str2 "[1,2,3]"))
    (is (equal '(1 2 3) (deserialize str2 :json :end (length str2))))
    (is (equal str2 (with-output-to-string (s) (serialize (list 1 2 3) :json :stream s))))))

(deftest xml ()
  (is (equal "foo" (xml-node-name (xml-parse "<foo></foo>")))))

(deftest toml ()
  "Tests based on https://github.com/toml-lang/toml-test"
  ;; comment
  ;; int
  ;; hex
  ;; octet
  ;; binary
  ;; float
  ;; bool
  ;; datetime
  ;; string
  ;; array
  ;; kv
  )

(defparameter *arff-input*
  "% 1. Title: Iris Plants Database
% 
% 2. Sources:
%      (a) Creator: R.A. Fisher
%      (b) Donor: Michael Marshall (MARSHALL%PLU@io.arc.nasa.gov)
%      (c) Date: July, 1988
% 
@RELATION iris
@ATTRIBUTE sepallength  NUMERIC
@ATTRIBUTE sepalwidth   NUMERIC
@ATTRIBUTE petallength  NUMERIC
@ATTRIBUTE petalwidth   NUMERIC
@ATTRIBUTE class        {Iris-setosa,Iris-versicolor,Iris-virginica}")

(deftest arff ()
  (is (typep (parse-arff-string *arff-input*) 'arff)))

(deftest bencode ())
