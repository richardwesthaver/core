;;; lib/dat/arff.lisp --- ARFF file format

;; Attribute-Relation File Format

;; based on https://github.com/pieterw/cl-arff-parser

;; ref: https://waikato.github.io/weka-wiki/formats_and_processing/arff_stable/

;; example header:
#|
% 1. Title: Iris Plants Database
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
@ATTRIBUTE class        {Iris-setosa,Iris-versicolor,Iris-virginica}
|#

;; example data:
#|
@DATA
5.1,3.5,1.4,0.2,Iris-setosa
4.9,3.0,1.4,0.2,Iris-setosa
4.7,3.2,1.3,0.2,Iris-setosa
4.6,3.1,1.5,0.2,Iris-setosa
5.0,3.6,1.4,0.2,Iris-setosa
5.4,3.9,1.7,0.4,Iris-setosa
4.6,3.4,1.4,0.3,Iris-setosa
5.0,3.4,1.5,0.2,Iris-setosa
4.4,2.9,1.4,0.2,Iris-setosa
4.9,3.1,1.5,0.1,Iris-setosa
|#

;; example datasets:
#|
https://storm.cis.fordham.edu/~gweiss/data-mining/datasets.html
|#
;;; Code:
(in-package :dat/arff)

(defclass arff ()
  ((arff-path :accessor arff-path
              :initarg :arff-path
              :initform "~/"
              :documentation "A string to the path of the arff
              file. e.g. /home/user/myData/foo.arff")
   (arff-relation :accessor arff-relation
                  :initarg :arff-reltation
                  :initform ""
                  :documentation "The string after @relation. This is
             essentially the name of the arff.")
   (arff-attributes :accessor arff-attributes
                    :initarg :arff-attributes
                    :initform nil 
                    :type list
                    :documentation "The attributes as specified in the
               header. Each attribute is a list that looks as
               follows: (\"attribute-name\" (\"type\")). In case of a
               nominal attribute it looks like
               this: (\"attribute-name\" (\"nominal\" . values)). ")
   (arff-data :accessor arff-data
              :initarg :arff-data
              :initform nil
              :type list
              :documentation "All the data. The bulk of the file."))
  (:documentation "An arff object contains all the data found in a
  parsed arff file."))

(defmethod print-object ((arff arff) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
        (format stream "<arff ~a:~%attributes: ~{~%~a~^,~}~%data: ~{~%~a~}>" 
                (arff-relation arff) (arff-attributes arff) (arff-data arff)))
      (format stream "<arff ~a>" (arff-relation arff))))

(defun trim-comments-and-spaces (string &optional (comment-marker "%"))
  (string-trim (list (code-char 9)) ;; tabs
               (string-trim " " (subseq string 0 (search comment-marker string)))))

(defun csv->list (string &optional (separator ","))
  "Given a string like '1,2,3, 6, foo' will return list ('1' '2' '3'
'6' 'foo')"
  (loop
    with continue = t
    with start = 0
    while continue
    for end = (search separator string :start2 start)
    unless end
      do (setf continue nil)
         (setf end (length string))
    collect (string-trim " " (subseq string start end))
    do (setf start (+ end 1))))

(defun string-replace (str1 sub1 sub2)
  "Nondestructively replaces all occurences of sub1 in str1 by sub2"
  (let ((str1 (string str1))
        (str2 "")
        (sub1 (string sub1))
        (sub2 (string sub2))
        (index1 0))
    (loop
      if (string-equal str1 sub1
                       :start1 index1
                       :end1 (min (length str1)
                                  (+ index1 (length sub1))))
        do (setq str2 (concatenate 'string str2 sub2))
           (incf index1 (length sub1))
      else do 
        (setq str2 (concatenate 'string str2
                                (subseq str1 index1 (1+ index1))))
        (incf index1)
      unless (< index1 (length str1))
        return str2)))

(defun search-space-or-tab (line)
  (or (search " " line)
      (search (list (code-char 9)) line)))

(defun parse-attribute-name (line)
  "Assumes the beginning of this line is the attribute-name. If spaces
are to be included in the name then the entire name must be quoted. As
second return value it also returns the rest of the line which should
be the datatype."
  (setf line (string-replace line (string (code-char 9)) " "))
  (if (and (search "'" line) ;; attribute name contains '
           (or (not (search "{" line))
               (< (search "'" line) (search "{" line))))
      (values (string-replace (subseq line 1 (search "'" line :start2 1)) " " "-")
	      (trim-comments-and-spaces (subseq line (1+ (search "'" line :start2 1)))))
      (values (subseq line 0 (search-space-or-tab line))
	      (trim-comments-and-spaces (subseq line (search-space-or-tab line))))))

(defun parse-datatype (line)
  "Assumes that the line starts with the datatype.Look at
http://www.cs.waikato.ac.nz/~ml/weka/arff.html for information about
the datatype. There is no support for the date datatype."
  (cond ((equal 0 (search "real" line :test #'string-equal))
	 (list "real"))
	((equal 0 (search "integer" line :test #'string-equal))
	 (list "integer"))
        ((equal 0 (search "numeric" line :test #'string-equal)) 
	 (list "numeric"))
        ((equal 0 (search "string" line :test #'string-equal))
	 (list "string"))
	((search "{" line) ;; nominal
	 (cons "nominal" 
	       (csv->list 
		(string-trim " " (subseq line (1+ (search "{" line)) (search "}" line))))))
	(t
	 (error "datatype ~a not real, integer or nominal" line))))


(defun parse-@attribute (line)
  "@attribute <attribute-name> <datatype>. Returns a list containing
the attribute-name and then a list containing datatype information as
parsed by parse-datatype."
  (let (attribute data-type)
    (multiple-value-setq (attribute data-type) 
      (parse-attribute-name (subseq line (1+ (search " " line)))))
    (list attribute (parse-datatype data-type))))

(defun parse-data (line)
  (csv->list line))


;; -----------------------------------------
;; Main function used to parse an arff file.
;; -----------------------------------------
(defun parse-arff-stream (stream &optional path)
  (let ((arff (make-instance 'arff :arff-path path)))
    (loop 
      with data-mode-p = nil ;; true when parsing data
      for line = (read-line stream nil)
      while line
      for trimmed-line = (trim-comments-and-spaces line)
      when (and data-mode-p
                (not (equalp trimmed-line "")))
        collect (parse-data line) into data
      when (not data-mode-p)
        do (cond ((equalp trimmed-line "")) ;; skip empty and commented lines
		 ((search "@relation" (string-downcase trimmed-line))
		  (setf (arff-relation arff) 
		        (subseq trimmed-line (1+ (search " " trimmed-line)))))
		 ((search "@attribute" (string-downcase trimmed-line))
		  (setf (arff-attributes arff)
		        (append (arff-attributes arff)
			        (list (parse-@attribute trimmed-line)))))
		 ((search "@data" (string-downcase trimmed-line))
		  (setf data-mode-p t)))
      finally (setf (arff-data arff) data))
    arff))

(defun parse-arff-string (string &optional path)
  (with-input-from-string (s string)
    (parse-arff-stream s path)))

(defun parse-arff (arff-path)
  "The arff-path should be a string pointing to an arff-file."
  (parse-arff-stream (open arff-path) arff-path))


(defgeneric remove-attribute-by-name (arff name)
  (:documentation "Removes the feature with the given name from the
  arff object (not from the actual file). It will remove it both from
  that @attributes and the @data."))

(defmethod remove-attribute-by-name ((arff arff) (name string))
  (let ((position (position name (arff-attributes arff) :key #'first :test #'string-equal)))
    (when position
      (setf (arff-attributes arff) 
	    (delete name (arff-attributes arff) :key #'first :test #'string-equal))
      (setf (arff-data arff)
	    (loop for instance in (arff-data arff)
	          collect (delete-if (nth position instance) instance))))))

(defmethod remove-attribute-by-name ((arff arff) name)
  (remove-attribute-by-name arff (format nil "~a" name)))
