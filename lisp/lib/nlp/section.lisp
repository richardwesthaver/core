(defpackage :nlp/section
  (:use :cl :std :nlp/doc :nlp/dbscan :nlp/tokenize)
  (:export :extract-sections))

(in-package :nlp/section)

(defun extract-sections (text &key (epsilon 0.5))
  "Extract the sections from a string of text. Epsilon refers to the
   distance between two points for them to be considered related."
  (labels ((average-distance (point points)
             (/ (reduce #'+ points
                        :key (lambda (i) (distance (vector-data i)
                                                   (vector-data point))))
                (length points))))
    (let ((collection (make-instance 'document-collection)))
      (loop for sentence in (sentence-tokenize text)
            do (add-document collection
                             (make-instance 'document-cluster
                                            :string-contents sentence)))
      (tf-vectorize-documents collection)
      (loop for document in (documents collection)
            with cluster-index = 0
            for cluster = (get-cluster cluster-index (documents collection))
            do (if (and cluster (>= epsilon (average-distance document cluster)))
                   (setf (cluster document) cluster-index)
                   (setf (cluster document) (incf cluster-index))))
      collection)))
