;;; nlp/pkg.lisp --- NLP packages

;;; Code:
(defpkg :nlp/data
  (:use :cl :std)
  (:export 
   :language-data
   :*language-data*
   :stop-words-lookup
   :stop-words))

(defpkg :nlp/stem/porter
  (:use :cl :std :rdb)
  (:export :stem))

(defpkg :nlp/tokenize
  (:use :cl :std :ppcre :nlp/data :nlp/stem/porter)
  (:export :word-tokenize :sentence-tokenize))

(defpkg :nlp/doc
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

(defpkg :nlp/textrank
  (:use :cl :std :nlp/doc :nlp/tokenize)
  (:export 
   :summarize-text :edges :document-vertex))

(defpkg :nlp/dbscan
  (:use :cl :std :nlp/doc :nlp/textrank :nlp/tokenize)
  (:export 
   :document-cluster :clusters :get-cluster :distance
   :generate-document-distance-vectors
   :cluster :neighbors :clusters
   :dbscan))

(defpkg :nlp/section
  (:use :cl :std :nlp/doc :nlp/dbscan :nlp/tokenize)
  (:export :extract-sections))

(defpkg :nlp/string
  (:use :cl)
  (:export    
   #:hamming
   #:levenshtein
   #:damerau-levenshtein
   #:norm-levenshtein
   #:norm-damerau-levenshtein
   #:overlap
   #:jaccard
   #:jaro
   #:jaro-winkler))

(defpkg :nlp/fuzzy
  (:use :cl)
  (:export :fuzzy-match :file-match))

