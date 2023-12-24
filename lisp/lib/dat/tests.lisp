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

(deftest xml ())

(deftest toml ()
  "Tests based on https://github.com/toml-lang/toml-test")

(deftest arff ())

(deftest bencode ())
