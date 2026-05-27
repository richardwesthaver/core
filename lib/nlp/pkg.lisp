;;; nlp/pkg.lisp --- NLP packages

;;; Code:
(defpkg :nlp/doc
  (:use :std-lisp :nlp/tokenize :graph)
  (:export :word-tokenize :sentence-tokenize :*language-data* :*stop-words*)
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
   :vector-data)
  (:export :summarize-text :edges :document-vertex)
  (:export 
   :document-cluster :clusters :get-cluster :distance
   :generate-document-distance-vectors
   :cluster :neighbors :clusters
   :dbscan)
  (:export :extract-sections)
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
  (:use :cl :nlp/doc)
  (:export :fuzzy-match :file-match))
