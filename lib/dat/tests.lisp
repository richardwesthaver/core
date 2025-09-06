(defpackage :dat/tests
  (:use :cl :std :rt :dat :log :ast)
  (:export))

(in-package :dat/tests)

(defsuite :dat)
(in-suite :dat)
(in-readtable :shell)

(deftest dot ()
  "Test Graphviz DOT functionality."
  (let ((g1 (make-instance 'graph:graph)))
    (graph:add-node g1 "foo")
    (graph:add-node g1 :bar)
    (graph:add-node g1 42)
    (graph:add-edge g1 '("foo" :bar) "a")
    (graph:add-edge g1 '(:bar 42) "b")
    (graph:add-edge g1 '(42 "foo") "c")
    (is (stringp (serialize g1 :dot)))
    (dat/dot::graph-to-dot-file g1 "/tmp/test")
    (is (probe-file "/tmp/test"))
    #$dot -Tsvg /tmp/test -o/tmp/test.svg$#
    (is (delete-file "/tmp/test"))
    (is (delete-file "/tmp/test.svg")))
  (serialize (graph:class-graph 'id:id) :dot :path "/tmp/class-graph-test")
  #$dot -Tsvg /tmp/test -o/tmp/class-graph-test.svg$#
  (is (delete-file "/tmp/class-graph-test")))


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
  (is (equal "foo" (xml-node-name (deserialize "<foo></foo>" :xml)))))

(deftest html ()
  (istype 'dat/html::document 
          (deserialize "<!DOCTYPE html><html lang=\"ulang\"></html>" :html)))

(deftest toml ()
  (istype 'dat/toml::toml-document
          (std:deserialize "[test] foo = true" :toml)))

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
(defparameter *svg-circle* 
  #"<svg width="800px" height="800px" viewBox="0 0 48 48" fill="none" xmlns="http://www.w3.org/2000/svg">
<path fill-rule="evenodd" clip-rule="evenodd" d="M24 34C29.5228 34 34 29.5228 34 24C34 18.4772 29.5228 14 24 14C18.4772 14 14 18.4772 14 24C14 29.5228 18.4772 34 24 34ZM24 36C30.6274 36 36 30.6274 36 24C36 17.3726 30.6274 12 24 12C17.3726 12 12 17.3726 12 24C12 30.6274 17.3726 36 24 36Z" fill="\#333333"/>
</svg>"#)

(deftest svg ()
  (let ((svg-list (svg:parse-svg-string *svg-circle*)))
    (istype 'list svg-list)
    (is svg-list)))

;;; TAR
(deftest tar ()
  (let ((path (tmpize-pathname "/tmp/foo.tar")))
    (with-open-tar-file (foo path :direction :output
                                  :if-does-not-exist :create)
      (istype 'tar-file foo)
      ;; (tar:finalize-tar-file foo)
      (istype 'tar-file-entry (tar::write-file-entry foo "bar" :data "a b c")))
    (with-open-tar-file (foo path :direction :input :type :auto)
      ;; (istype 'tar-file-entry (read-entry foo))
      (istype 'v7-tar-file foo))
    (is (delete-file path))))

;; FIX 2024-12-27: 
(deftest tar-zst (:skip t)
  (let ((path (format nil "/tmp/~A.tar.zst" (gensym "foo"))))
    (with-open-tar-file (foo path :direction :output :type 'v7-tar-file
                         :if-exists :overwrite
                         :if-does-not-exist :create
                         :compression :zstd)
      (istype 'tar-file foo))))

;;; INI
(deftest ini ()
  (let ((str "[window]
width=956
height=1025
sinkInputType=0
sourceOutputType=1
sinkType=0
sourceType=1
showVolumeMeters=1"))
    (istype 'dat/ini:ini-document (deserialize str :ini))))
