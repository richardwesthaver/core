(defpackage :dat/tests
  (:use :cl :std :rt :dat :log))

(in-package :dat/tests)

(defsuite :dat)
(in-suite :dat)
(in-readtable :std)

(deftest bytes ())

(deftest dot ()
  "Test Graphviz DOT functionality."
  (let ((g1 (make-instance 'graph:graph)))
    (graph:add-node g1 "foo")
    (graph:add-node g1 :bar)
    (graph:add-node g1 42)
    (graph:add-edge g1 '("foo" :bar) "a")
    (graph:add-edge g1 '(:bar 42) "b")
    (graph:add-edge g1 '(42 "foo") "c")
    (is (stringp (dat/dot::to-dot g1)))
    (dat/dot::to-dot-file g1 "/tmp/test")
    (is (probe-file "/tmp/test"))
    (is (delete-file "/tmp/test"))))

(deftest csv ()
  "Test CSV functionality."
  (is
   (not
    (sequence:emptyp 
     (with-output-to-string (str)
       (dat/csv:write-csv-stream
        str
        #(#(1 2 3 4) #(2 3 4 5) #(3 4 5 6))))))))

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

(deftest sxp-string ()
  (let ((f (make-instance 'ast:ast)))
    (is (ast::formp (read-sxp-string f *sxp-test-short*)))
    (is (equalp (read-from-string (write-sxp-string f)) (read-from-string *sxp-test-short*)))))

(deftest sxp-stream ()
  (let ((f (sxp:make-sxp)))
    (with-input-from-string (s *sxp-test-long*)
      (read-sxp-stream f s))
    (with-output-to-string (s)
      (is (write-sxp-stream f s)))))

(defparameter *parquet-test-file*
  (probe-file
   (merge-pathnames "../../../.stash/alltypes_plain.parquet"
                    #.(asdf:system-source-directory :dat/tests))))
;; see also: https://github.com/apache/parquet-testing/blob/master/data/README.md
(deftest parquet-basic ()
  (when *parquet-test-file*
    (with-open-file (st *parquet-test-file* :element-type '(unsigned-byte 8))
      (let ((footer (dat/parquet::parquet-read-footer st)))
        (is (typep footer
                   'dat/parquet::parquet-file-meta-data))
        (info! (slot-value footer 'dat/parquet::schema))
        (info! (file-position st))
        (info! (file-length st))))))

;;; PNG
;; TODO 2024-10-26: 
(deftest png ())

;;; SVG
;; FIX 2024-10-26: move to packy test files
(defparameter *svg-file* "~/.stash/simplex16.svg")

(deftest svg ()
  (istype 'list (svg:parse-svg-file *svg-file*)))

;;; TAR
(deftest tar ()
  (let ((path (tmpize-pathname "/tmp/foo.tar")))
    (with-open-tar-file (foo path :direction :output :type 'v7-tar-file
                                      :if-exists :overwrite
                                      :if-does-not-exist :create)
      (istype 'tar-file foo)
      ;; (tar:finalize-tar-file foo)
      (istype 'tar-file-entry (tar::write-file-entry foo "bar" :data "a b c")))
    (with-open-tar-file (foo path :direction :input :type :auto)
      (istype 'tar-file-entry (read-entry foo))
      (istype 'v7-tar-file foo))
    (is (delete-file path))))

(deftest tar-zst (:skip t)
  (let ((path (format nil "/tmp/~A.tar.zst" (gensym "foo"))))
    (with-open-tar-file (foo path :direction :output :type 'v7-tar-file
                         :if-exists :overwrite
                         :if-does-not-exist :create
                         :compression :zstd)
      (istype 'tar-file foo))))
