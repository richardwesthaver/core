(defpackage :nlp/data
  (:use :cl :std)
  (:export 
   :language-data
   :*language-data*
   :stop-words-lookup
   :stop-words))

(defpackage :nlp/stem/porter
  (:use :cl :std :rdb)
  (:export :stem))

(defpackage :nlp/tokenize
  (:use :cl :std :cl-ppcre :nlp/data :nlp/stem/porter)
  (:export :word-tokenize :sentence-tokenize))

(defpackage :nlp/doc
  (:use :cl :std :nlp/data :nlp/tokenize)
  (:export 
   :document
   :documents
   :add-document
   :document-collection
   :keywords
   :dictionary
   :term-count
   :document-frequency
   :inverse-document-frequency
   :tf-idf-vectorize-documents
   :termp
   :string-contents
   :rank
   :term-frequency
   :extract-keywords
   :tf-vectorize-documents
   :vector-data))

(defpackage :nlp/textrank
  (:use :cl :std :nlp/doc :nlp/tokenize)
  (:export 
   :summarize-text :edges :document-vertex))

(defpackage :nlp/dbscan
  (:use :cl :std :nlp/doc :nlp/textrank :nlp/tokenize)
  (:export 
   :document-cluster :clusters :get-cluster :distance
   :generate-document-distance-vectors
   :cluster :neighbors :clusters
   :dbscan))

(defpackage :nlp/section
  (:use :cl :std :nlp/doc :nlp/dbscan :nlp/tokenize)
  (:export :extract-sections))

(defpackage :nlp/fuzzy
  (:use :cl)
  (:export :fuzzy-match))

(uiop:define-package :nlp/pkg
  (:nicknames :nlp)
  (:use-reexport
   :nlp/data
   :nlp/tokenize
   :nlp/doc
   :nlp/stem/porter
   :nlp/textrank
   :nlp/dbscan
   :nlp/section))
