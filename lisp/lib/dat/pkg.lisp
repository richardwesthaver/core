;;; dat/pkg.lisp --- Data

;;; Code:
(defpackage :dat/proto
  (:use :cl :std)
  (:export :serialize :deserialize
           :serializer-error :deserializer-error
           :serde :serde-error :dat-error))

(defpackage :dat/sxp
  (:nicknames :sxp)
  (:use :cl :sb-mop :std)
  (:import-from :uiop :read-file-forms :slurp-stream-forms :with-output-file)
  ;; TODO: hot-patch readtables into sxp classes/parsers
  (:import-from :std/named-readtables :defreadtable :in-readtable)
  (:export
   :sxp-fmt-designator
   :form :formp :sxp-error :sxp-fmt-error :sxp-syntax-error :reader :writer :fmt
   :wrap :wrap! :wrap-from-string! :unwrap :unwrap! :unwrap-or
   :sxpp :build-ast :load-ast :ast
   :define-macro :define-fmt :read-sxp-file :write-sxp-file
   :read-sxp-string :write-sxp-string :read-sxp-stream :write-sxp-stream
   :make-sxp :sxp :formp :form
   :file-read-forms
   :wrap-object :unwrap-object))

(defpackage :dat/csv
  (:use :cl :std :dat/proto)
  (:export
   :read-csv-file
   :*csv-separator*
   #:read-csv-stream
   :write-csv-file
   :write-csv-stream
   :read-csv-file-and-sort
   :parse-csv-string))

(defpackage :dat/toml
  (:use :cl :std :dat/proto :obj/time)
  (:export
   #:parse
   #:parse-toml-blocks
   #:toml-collection
   #:children
   #:toml-table
   #:inline-toml-table
   #:toml-table-array
   #:toml-redefine-table-error
   #:toml-redefine-property-error
   #:toml-modify-inline-table-error
   #:toml-dotted-key-redefine-table-error
   #:toml-dotted-key-open-table-array-error))

(defpackage :dat/arff
  (:use :cl :std :dat/proto :dat/csv)
  (:export
   :arff :arff-relation :arff-attributes :arff-data :arff-path 
   :parse-arff :parse-arff-string :parse-arff-stream
   :remove-attribute-by-name))

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

(defpackage :dat/html
  (:use :cl :std :dat/proto)
  (:import-from :sb-ext :defglobal)
  (:export
   :parse-html5
   :parse-html5-fragment
   :transform-html5-dom
   :xml-escape-name
   :xml-unescape-name
   ;; A simple DOM
   :make-document
   :make-fragment
   :make-doctype
   :make-comment
   :make-element
   :make-text-node
   :node-type
   :node-name
   :node-namespace
   :node-value
   :node-public-id
   :node-system-id
   :element-attribute
   :node-append-child
   :node-insert-before
   :node-remove-child
   :node-parent
   :node-first-child
   :node-last-child
   :node-previous-sibling
   :node-next-sibling
   :element-map-attributes
   :element-map-attributes*
   :element-map-children))

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

(defpackage :dat/midi
  (:nicknames :midi)
  (:use :cl :std :dat/proto)
  (:export #:read-midi-file #:write-midi-file
           #:midifile
           #:midifile-format #:midifile-tracks #:midifile-division
           #:message #:note-off-message #:note-on-message #:tempo-message
           #:program-change-message #:pitch-bend-message
           #:key-signature-message #:time-signature-message
           #:smpte-offset-message
           #:sequence/track-name-message
           #:message-channel #:message-key #:message-time
           #:message-velocity #:message-numerator #:message-denominator
           #:message-sf #:message-mi #:message-tempo #:message-program
           #:message-value
           #:header #:header-type
           #:unknown-event #:status #:data-byte #:dd #:bb #:cc #:nn))

(defpackage :dat/qrcode
  (:nicknames :qrcode)
  (:use :cl :std :dat/proto)
  (:export
   :encode-symbol
   ;; QR code representation
   ;; this should be enough to write another backend for QR symbol
   :qr-symbol
   :matrix
   :modules
   :dark-module-p
   :read-file-content))

(defpackage :dat/gif
  (:nicknames :gif)
  (:use :cl :std :dat/proto))

(defpackage :dat/png
  (:use :cl :std :dat/proto :png))

(uiop:define-package :dat
    (:use-reexport :dat/proto :dat/csv :dat/arff :dat/toml :dat/json :dat/sxp :dat/xml :dat/bencode
                   ;; :dat/qrcode
                   ))
