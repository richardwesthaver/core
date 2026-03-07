(defpackage :dat/tests
  (:use :cl :std :rt :dat :log :ast :dat/html :dat/css :dat/tar :dat/ttf :dat/img :dat/gif :color)
  (:export))

(in-package :dat/tests)

(defsuite :dat)
(in-suite :dat)
(in-readtable :shell)

(deftest dot ()
  "Test Graphviz DOT functionality."
  (let ((g1 (make-instance 'graph:simple-graph)))
    (graph:add-node g1 "foo")
    (graph:add-node g1 :bar)
    (graph:add-node g1 42)
    (graph:add-edge g1 '("foo" :bar) "a")
    (graph:add-edge g1 '(:bar 42) "b")
    (graph:add-edge g1 '(42 "foo") "c")
    (is (stringp (with-output-to-string (s) (serialize g1 :dot :stream s))))
    (dat/dot::graph-to-dot-file g1 "/tmp/test" :edge-attrs nil)
    (is (probe-file "/tmp/test"))
    #$dot -Tsvg /tmp/test -o/tmp/test.svg$#
    (is (delete-file "/tmp/test"))
    (is (delete-file "/tmp/test.svg"))))

(deftest class-graph-dot ()
  (serialize (graph:class-graph 'id:id) :dot :path "/tmp/class-graph-test")
  #$dot -Tsvg /tmp/class-graph-test -o/tmp/class-graph-test.svg$#
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
    (multiple-value-bind (res pos) (deserialize str :json)
      (is (equal obj res))
      (is (= pos 29)))
    (is (equal str (with-output-to-string (s) (serialize obj :json :stream s)))))
  (let ((str2 "[1,2,3]"))
    (is (equal '(1 2 3) (deserialize str2 :json :end (length str2))))
    (is (equal str2 (with-output-to-string (s) (serialize (list 1 2 3) :json :stream s))))))

(deftest xml ()
  (isequal "foo" (name (deserialize "<foo>foo</foo>" :xml))))

(deftest html-to-xml ()
  (let ((test-doc (deserialize 
                   "<!DOCTYPE html><html lang=\"ulang\"><body><div><p>henlo</p></div></body></html>"
                   :html)))
    (istype 'dat/html::document test-doc)
    (let ((sxp (serialize test-doc :xml)))
      (istype 'list sxp)
      (let ((xmlrep (dat/xml::make-xmlrep "html" :children sxp)))
        (istype 'dat/xml:xml-node xmlrep)
        (is (dat/xml:xmlrep-find-child-tag :body xmlrep))
        (is (dat/xml:xmlrep-tagmatch :html xmlrep))))))

;; TODO 2025-09-29: 
(deftest with-html ()
  (isequalp (with-html-string (:a :href "foo"))
            "<!DOCTYPE html>
<a href='foo'></a>")
  (setf *html-indent* 0)
  (isequal
   (with-html-string 
     (htm (:a :href "foo")))
   "<!DOCTYPE html>
<a href='foo'></a>"))

(deftest css ()
  (isequal 
   ".PROJECT { color: lighseagreen; font-weight: bold; }
" 
   (css '((".PROJECT" :color lighseagreen :font-weight bold)))))

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
   (merge-pathnames "../../../.stash/alltypes_plain.parquet" (system-home :dat))))

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
(defun draw-rgb (file)
  (let ((png (make-instance 'png:pixel-streamed-png
                             :color-type :truecolor-alpha
                             :width 200
                             :height 200)))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (png:start-png png stream)
      (loop for a from 38 to 255 by 31
	do (loop for b from 10 to 255 by 10
	     do (loop for g from 38 to 255 by 31
		  do (loop for r from 10 to 255 by 10
			do (png::write-pixel (list r g b a) png)))))
      (png:finish-png png))))

(deftest png ()
  (is (draw-rgb "/tmp/foo.png")))

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

;;; TTF
(deftest ttf ()
  (init :ttf)
  (is (get-font-families))
  (is (get-font-subfamilies (car (get-font-families)))))

;;; GIF
(deftest gif1 ()
  ;; example1
  (let* ((height 100)
         (width 100)
         (stream (make-gif-stream 
                  :height height
                  :width width
                  :color-table t))
         (image (make-gif-image :height height :width width))
         (red (ensure-color (rgb-color 1 0 0)
                            (color-table stream)))
         (white (ensure-color (rgb-color #xFF #xFF #xFF)
                              (color-table stream))))
    (add-image image stream)
    (fill (data image) white)
    (dotimes (i (truncate height 2))
      (let* ((start (* i width 2))
             (end (+ start width)))
        (fill (data image) red :start start :end end)))
    (serde stream #p"/tmp/test.gif")
    (is (probe-file #p"/tmp/test.gif"))
    (is (delete-file #p"/tmp/test.gif"))))

(deftest gif2 ()
  (let* ((height 9)
         (width 99)
         (color-table (make-color-table))
         (data-stream (make-gif-stream :height height
                                        :width width
                                        :color-table color-table))
         (gray (ensure-color #xCCCCCC color-table))
         (white (ensure-color #xFFFFFF color-table))
         (black (ensure-color #x000000 color-table))
         (bg (make-gif-image :stream data-stream
                         :width width :height height
                         :data (make-image-data 
                                height width
                                :initial-element gray)))
         (sprite-data (make-image-data 3 3)))
    (flet ((hatch-data (data a b)
             (dotimes (i (length data))
               (setf (aref data i) (if (zerop (mod i 2)) a b)))))
      (hatch-data sprite-data white black)
      (hatch-data (data bg) white gray)
      (dotimes (i 128)
        (add-color (random #xFFFFF) color-table))
      (dotimes (i 96)
        (let ((image (make-gif-image :height 3
                                 :width 3
                                 :data sprite-data
                                 :top 3
                                 :delay-time 10
                                 :disposal-method :restore-previous
                                 :transparency white
                                 :left i)))
          (add-image image data-stream)))
      (setf (loopingp data-stream) t)
      (with-directory "/tmp/"
        (is (output-gif-stream data-stream #p"example2.gif"))
        (is (delete-file #p"example2.gif"))))))

(deftest gif3 ()
  (let* ((height 100)
         (width 100)
         (color-count 256)
         (color-table (make-color-table))
         (data-stream (make-gif-stream :color-table color-table
                                        :loopingp t
                                        :height height
                                        :width width)))
    (dotimes (i color-count)
      (add-color (rgb-color (random 255) (random 255) (random 255))
                 color-table))
    (dotimes (i color-count)
      (let* ((top (random height))
             (left (random width))
             (h (1+ (random (- height top))))
             (w (1+ (random (- width left))))
             (image (make-gif-image :height h
                                :width w
                                :stream data-stream
                                :top top
                                :left left
                                :data (make-image-data 
                                       w h
                                       :initial-element (random color-count))
                                :delay-time 5)))
        (add-image image data-stream)))
      (with-directory "/tmp/"
        (is (output-gif-stream data-stream #p"example3.gif"))
        (is (delete-file #p"example3.gif")))))
