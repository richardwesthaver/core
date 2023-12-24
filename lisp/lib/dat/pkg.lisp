(defpackage :dat/proto
  (:use :cl :std)
  (:export :serialize :deserialize))

(defpackage :dat/csv
  (:use :cl :std :dat/proto)
  (:export
   :read-csv-file
   #:read-csv-stream
   :write-csv-file
   :write-csv-stream
   :read-csv-file-and-sort
   :parse-csv-string))

(defpackage :dat/arff
  (:use :cl :std :dat/proto :dat/csv)
  (:export
   :arff :arff-relation :arff-attributes :arff-data :arff-path 
   :parse-arff :parse-arff-string :parse-arff-stream
   :remove-attribute-by-name))

(defpackage :dat/toml
  (:use :cl :std :dat/proto :obj/time)
  (:export
   #:parse
   #:parse-toml-blocks
   #:collection
   #:children
   #:table
   #:inline-table
   #:table-array
   #:toml-redefine-table-error
   #:toml-redefine-property-error
   #:toml-modify-inline-table-error
   #:toml-dotted-key-redefine-table-error
   #:toml-dotted-key-open-table-array-error))

(defpackage :dat/json
  (:use :cl :std :dat/proto)
  (:export
   #:json-decode
   #:json-encode
   #:json-read
   #:json-enable-reader-macro
   #:json-object
   #:json-object-members
   #:json-getf
   #:json-setf))

(defpackage :dat/xml
  (:use :cl :std :dat/proto)
  (:shadow :read-char :unread-char)
  (:export
   :xml-node-name 
   :xml-node-ns 
   :xml-node-attrs 
   :xml-node-children 
   :make-xml-node 
   :xml-parse 
   :to-xml 
   :write-xml
   :xml-node-p 
   :nodelist->xml-node
   :xml-node->nodelist
   :xml-node ; needed to support use in typep
   ;; processing instruction objects
   :proc-inst-p
   :proc-inst-target
   :proc-inst-contents
   :write-prologue
   :write-prolog
   ;; rpg utils
   :make-xmlrep :xmlrep-add-child!
   :xmlrep-tag :xmlrep-tagmatch
   :xmlrep-attribs :xmlrep-children
   :xmlrep-string-child :xmlrep-integer-child
   :xmlrep-find-child-tags :xmlrep-find-child-tag
   :xmlrep-attrib-value :xmlrep-boolean-attrib-value
   ;; tree searching from Daniel Eliason
   :extract-path-list
   :extract-path))

(defpackage :dat/bencode
  (:use :cl :std :dat/proto :sb-gray)
  (:export
   :bencode-encode
   :bencode-decode
   :*bencode-binary-key-p*))

(defpackage :dat/ical
  (:use :cl :std :dat/xml))

(defpackage :dat/vcard
  (:use :cl :std :dat/xml))

(uiop:define-package :dat
    (:use-reexport :dat/proto :dat/csv :dat/arff :dat/toml :dat/json :dat/xml :dat/bencode))
