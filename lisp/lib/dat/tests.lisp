(defpackage :dat/tests
  (:use :cl :std :rt :dat))

(in-package :dat/tests)

(defsuite :dat)
(in-suite :dat)
(in-readtable :std)

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

(defvar *sxp-test-long* "(edges-1
(
(1389.886593 1341.567282)
(1383.122623 1339.369530)
)
(
(1383.122623 1339.369530)
(1387.706464 1325.261939)
)
(
(1387.706464 1325.261939)
(1394.470360 1327.459664)
)
(
(1394.470360 1327.459664)
(1389.886593 1341.567282)
)
) ; edges end

(edges-2
( ( 1.1 2.2 ) (2.2 3.3) )
( ( 2.2 3.3 ) (3.3 3.3) )
( ( 3.3 3.3 ) (1.1 2.2) )
) ; end edges of triangle room

(= 4 4)
(= 5 4)
(> 4.0 54.0)
(= 4 s)
(= (= 4 4) (> 5 4))
(not (= 3 3))
(not 4)
(if (= 4 4) 42 666)
(if (= 4.0 4.0) (42))
(+ 4 4)
(+ 5.0 6.5)
(- 4 5)
(^ 2 3)
(^ 3 2)
(^ 3 (+ 2 1))
")

(defvar *sxp-test-short* "(FOO 'BAR `(\"test\" ,BAZ ,@QUX) 123 0.0123 1/3 `(,A1 ,A2))")

(deftest forms ()
  (is (formp nil))
  (is (formp t))
  (is (formp 3.14))
  (is (formp "string"))
  (is (formp (mapc #`(',a1) '(a))))
  (is (formp ())))

(deftest sxp-string ()
  (let ((f (make-instance 'sxp)))
    (is (formp (read-sxp-string f *sxp-test-short*)))
    (is (equalp (read-from-string (write-sxp-string f)) (read-from-string *sxp-test-short*)))))

(deftest sxp-stream ()
  (let ((f (make-instance 'sxp)))
    (with-input-from-string (s *sxp-test-long*)
      (read-sxp-stream f s))
    (with-output-to-string (s)
      (is (write-sxp-stream f s)))))
