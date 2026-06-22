;;; lib/doc/pkg.lisp --- Documentation

;; This package is designed to help us navigate our Lisp systems,
;; packages, symbols, and files to extract information relevant to
;; documentation.

;;; Commentary:

;; Here are some of the categories of information we're interested in:

;; - Comments :: like this one.
#| or this one |#

;; - Docstrings :: typically store in symbol properties, documentation
;;   metaclass slot, etc. often found somewhere in the body of a form
;;   starting with DEF.

;; - Object Structure :: for functions - their declared type, for
;;   objects their slots, methods, sub/superclasses, allocation info,
;;   etc.

;; - Source :: the source code which defines a symbol and its
;;   file/line location.

;; Documentation is a tricky craft, good thing we have a
;; self-documenting language :).

;;; Code:
(defpkg :doc/nlp
  (:nicknames :nlp)
  (:use :std-lisp :graph :ast)
  (:export :word-tokenize :sentence-tokenize :*language-data* :*stop-words*)
  (:export 
   :search-document
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
  (:export :stem)
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

(defpkg :doc/fuzzy
  (:nicknames :fuzzy)
  (:use :cl :doc/nlp)
  (:export :fuzzy-match :file-match))

(defpkg :doc
  (:use :cl :std :organ :sb-mop :sb-introspect :id :log :ast :project :config :val :tempo :srv :uuid)
  (:import-from :uiop :string-prefix-p)
  (:import-from :sb-c :packed-info :symbol-hash :symbol-dbinfo :vop-p :package-external-symbol-count)
  (:import-from :sb-kernel :symbol-package-id)
  (:import-from :sb-ext :restrict-compiler-policy)
  (:import-from :sb-impl :print-standard-describe-header :describe-object)
  (:import-from :sb-int :condition)
  (:import-from :sb-alien :alien-type-p)
  (:use-reexport :doc/nlp :doc/fuzzy)
  (:export
   :*document-class*
   :*definition-types*
   :definition-specifier
   :find-definitions
   :definition-source-line-number
   :classify-symbol :symbol-classification-string
   :fmt-tags :symbol-tag-string
   :file-commentary
   :file-summary
   :file-description
   :file-heading :file-headline :file-header :read-file-header
   :summary :commentary
   :read-file-outline
   :+max-heading-level+ :+min-heading-level+
   :make-file-header 
   :make-shebang-file-header 
   :make-source-file-header 
   :file-header-kind
   :file-header
   :make-source-header-comment 
   :make-shebang-comment
   :file-documentation
   :system-documentation
   ;; :image-documentation
   :package-documentation
   :symbol-documentation
   :symbol-info
   :doc
   :document-class
   :publish
   :doc-files
   :doc-symbols
   :doc-object
   :doc-packages
   :doc-systems
   :doc-components
   :print-doc
   :print-documentation
   :asdf-system-documentation
   :project-documentation))
