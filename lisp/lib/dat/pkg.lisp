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
  (:export))

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
  (:export))

(defpackage :dat/bencode
  (:use :cl :std :dat/proto :sb-gray)
  (:export
   :bencode-encode
   :bencode-decode
   :*bencode-binary-key-p*))

(uiop:define-package :dat
  (:use-reexport :dat/proto :dat/csv :dat/arff :dat/toml :dat/json :dat/xml :dat/bencode))
