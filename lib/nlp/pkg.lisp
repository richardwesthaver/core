;;; nlp/pkg.lisp --- NLP packages

;;; Code:
(defpkg :nlp/fuzzy
  (:use :cl)
  (:export :fuzzy-match :file-match))

(defpkg :nlp/stem/porter
  (:use :cl :std :rdb)
  (:export :stem))

(defpkg :nlp/tokenize
  (:use :cl :std :ppcre :nlp/stem/porter)
  (:export :word-tokenize :sentence-tokenize :*language-data* :*stop-words*))

(defpkg :nlp/doc
  (:use :std-lisp :nlp/tokenize :graph)
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
