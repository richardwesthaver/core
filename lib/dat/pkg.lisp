;;; dat/pkg.lisp --- Data

;;; Code:
(defpackage :dat/proto
  (:use :cl :std)
  (:export :dat-error))

(defpackage :dat/sxp
  (:nicknames :sxp)
  (:use :cl :sb-mop :std :ast)
  (:import-from :uiop :read-file-forms :slurp-stream-forms :with-output-file)
  (:import-from :std/named-readtables :defreadtable :in-readtable)
  (:export
   :sxp-fmt-designator
   :sxpp
   :read-sxp-file :write-sxp-file
   :read-sxp-string :write-sxp-string
   :make-sxp
   :file-read-forms))

(defpackage :dat/dot
  (:nicknames :dot)
  (:use :cl :std :dat/proto :obj/graph)
  (:export
   :read-dot-file :write-dot-file
   :read-dot-stream :write-dot-stream
   :parse-dot-string))

(defpackage :dat/csv
  (:nicknames :csv)
  (:use :cl :std :dat/proto :obj/query)
  (:export
   :read-csv-file
   :*csv-separator*
   #:read-csv-stream
   :write-csv-file
   :write-csv-stream
   :read-csv-file-and-sort
   :parse-csv-string
   #:write-csv-string))

(defpackage :dat/ini
  (:nicknames :ini)
  (:use :cl :std :dat/proto :ast)
  (:export
   :ini-document
   :ini-read
   :ini-section
   :ini-object))

(defpackage :dat/toml
  (:nicknames :toml)
  (:use :cl :std :dat/proto :time :ast)
  (:export
   #:parse-toml-blocks
   :parse-toml-value
   #:toml-collection
   #:children
   #:toml-table
   #:inline-toml-table
   #:toml-table-array
   #:toml-redefine-table-error
   #:toml-redefine-property-error
   #:toml-modify-inline-table-error
   #:toml-dotted-key-redefine-table-error
   #:toml-dotted-key-open-table-array-error
   #:toml-document))

(defpackage :dat/arff
  (:nicknames :arff)
  (:use :cl :std :dat/proto :dat/csv)
  (:export
   :arff :arff-relation :arff-attributes :arff-data :arff-path 
   :parse-arff :parse-arff-string :parse-arff-stream
   :remove-attribute-by-name))

(defpackage :dat/json
  (:nicknames :json)
  (:use :cl :std :dat/proto)
  (:import-from :ast :ast)
  (:export
   :*allow-json-trailing-commas*
   :json-trailing-commas-p
   #:json-decode
   #:json-encode
   #:json-read
   #:json-enable-reader-macro
   #:json-object
   #:json-getf
   #:json-setf
   #:json-write))

(defpackage :dat/base64
  (:use :cl :std :dat/proto)
  (:export #:base64-stream-to-integer
           #:base64-stream-to-string
           #:base64-stream-to-stream
           #:base64-stream-to-usb8-array
           #:base64-string-to-integer
           #:base64-string-to-string
           #:base64-string-to-stream
           #:base64-string-to-usb8-array
           #:string-to-base64-string
           #:string-to-base64-stream
           #:usb8-array-to-base64-string
           #:usb8-array-to-base64-stream
           #:stream-to-base64-string
           #:stream-to-base64-stream
           #:integer-to-base64-string
           #:integer-to-base64-stream
           ;; Conditions.
           #:base64-error
           #:bad-base64-character
           #:incomplete-base64-data
           ;; For creating custom encode/decode tables.
           #:make-decode-table
           #:+decode-table+
           #:+uri-decode-table+))

(defpackage :dat/m3u
  (:nicknames :m3u)
  (:use :cl :std :dat/proto)
  (:export
   :read-m3u-file
   :read-m3u-stream
   :write-m3u-file
   :write-m3u-stream
   :parse-m3u-string
   :write-m3u-string))

(defpackage :dat/id3
  (:use :cl :std :dat/proto :id)
  (:export
   :read-id3
   :mp3-p
   :id3-p
   :decode-u28
   :read-id3-header
   :show-id3-header
   :show-id3-headers))

(defpackage :dat/css
  (:use :cl :std :dat/proto)
  (:export
   :parse-css
   :*minify-css*
   :*css-indent-offset*
   :parse-css-selector
   :parse-css-fragment
   :generate-css
   :compile-css-selector
   :compile-css
   :compile-css-block))

(defpackage :dat/html
  (:use :cl :dat/proto :std/macs :std/string :std/serde)
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
  (:use :cl :dat/proto)
  (:import-from :log :trace!)
  (:import-from :std :serialize :deserialize)
  (:shadow :read-char :unread-char)
  (:export
   :xml-node-name 
   :xml-node-ns 
   :xml-node-attrs 
   :xml-node-children 
   :make-xml-node 
   :xml-parse 
   :write-xml
   :xml-node-p 
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

(defpackage :dat/handlebars
  (:use :cl :std :dat/proto :dat/html)
  (:export))
  
(defpackage :dat/mime
  (:use :cl :std :dat/proto :dat/xml)
  (:export :*mime-database*
   :load-mime-info :update-mime-database
   :mime-magic :mime-magic-offset :mime-magic-type :mime-magic-value
   :mime-type :mime-type-name :mime-type-superclasses :mime-type-glob
   :mime-type-glob :mime-type-magic
   :mime :get-mime :get-mime*))

(defpackage :dat/yaml
  (:use :cl :dat/proto)
  (:import-from :log :trace!))

(defpackage :dat/midi
  (:nicknames :midi)
  (:use :cl :std :dat/proto :io/proto)
  (:export #:read-midi-file #:write-midi-file
           #:midifile
           #:midifile-format #:midifile-tracks #:midifile-division
           #:note-off-message #:note-on-message #:tempo-message
           #:program-change-message #:pitch-bend-message
           #:key-signature-message #:time-signature-message
           #:smpte-offset-message
           #:sequence/track-name-message
           #:message-channel #:message-key #:message-time
           #:message-velocity #:message-numerator #:message-denominator
           #:message-sf #:message-mi #:message-tempo #:message-program
           #:message-value
           #:unknown-event #:data-byte #:dd #:bb #:cc #:nn))

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
  (:use :cl :std :dat/proto)
  (:export))

(defpackage :dat/png
  (:use :cl :std :dat/proto #+png :png)
  (:export))

(defpackage :dat/tar
  (:nicknames :tar)
  (:use :cl :std :dat/proto 
   :io/proto :io/chunky :sb-ext :io/stream
   :std/stream :std/macs :std/ht :io/flate)
  (:export
   #:*tar-block-bytes*
   #:tar-error
   #:simple-tar-error
   #:close-tar-file
   #:tar-file
   #:read-entry
   #:write-entry
   #:open-tar-file
   #:transfer-stream-to-tar-file
   #:transfer-octets-to-tar-file
   #:finalize-tar-file
   #:v7-tar-file
   #:entry-type
   #:ustar-tar-file
   #:gnu-tar-file
   #:archive
   #:tar-archive
   #:tar-entry
   #:tar-entry-data
   #:tar-file-entry
   #:tar-hard-link-entry
   #:tar-symbolic-link-entry
   #:tar-character-device-entry
   #:write-character-device-entry
   #:tar-block-device-entry
   #:tar-directory-entry
   #:tar-fifo-entry
   #:pax-attributes-entry
   #:do-attributes
   #:read-attribute
   #:pax-extended-attributes-entry
   #:pax-global-attributes-entry
   #:gnu-directory-dump-entry
   #:gnu-long-link-name-entry
   #:gnu-long-name-entry
   #:gnu-sparse-file-entry
   #:gnu-volume-header-name-entry
   #:unknown-tar-entry
   #:entry-unknown-p
   #:entry-has-data-p
   #:make-entry-stream
   #:entry-file-p
   #:entry-directory-p
   #:entry-hard-link-p
   #:entry-symbolic-link-p
   #:entry-character-device-p
   #:entry-block-device-p
   #:entry-fifo-p
   #:call-with-open-tar-file
   #:with-open-tar-file
   #:do-entries))

(defpackage :dat/parquet
  (:use :cl :std :obj/id :dat/proto :dat/json)
  (:export
   :parquet-object
   :parquet-enum-object
   :parquet-struct-object
   :parquet-read
   :parquet-write
   :parquet-encode
   :parquet-decode))
