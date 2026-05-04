;;; dat/pkg.lisp --- Data

;;; Code:
(defpkg :dat/img
  (:use :std-lisp)
  (:import-from :io :io-stream)
  (:export 
   :image :images
   :canvas :scale
   :write-canvas :read-canvas 
   :save-canvas :load-canvas 
   :fill-canvas :clip-canvas
   :flip-horizontal :flip-vertical
   :rotate-180 :pixel
   :image-error :image-condition
   :image-stream :check-dimensions
   :last-image :add-image
   :make-image-data
   :transparentp
   :check-image-dimensions
   :left :top :width :height))

(defpkg :dat/asn1
  (:nicknames :asn1)
  (:use :cl :std)
  (:import-from :openssl :v-asn1 :asn1-string-type
   :asn1-time-check :asn1-utctime-check :asn1-string-data :asn1-string-length)
  (:export #:decode-asn1-string #:try-get-asn1-string-data #:decode-asn1-time))

(defpkg :dat/dot
  (:nicknames :dot)
  (:use :cl :std :obj/graph)
  (:export :graph-to-dot-file :graph-from-dot-file :graph-to-dot))

(defpkg :dat/csv
  (:nicknames :csv)
  (:use :cl :std)
  (:export
   :read-csv-file
   :*csv-separator*
   #:read-csv-stream
   :write-csv-file
   :write-csv-stream
   :read-csv-file-and-sort
   :parse-csv-string
   #:write-csv-string))

(defpkg :dat/ini
  (:nicknames :ini)
  (:use :cl :std :ast)
  (:export
   :ini-document
   :ini-read
   :ini-section
   :ini-object
   :desktop-entry))

(defpkg :dat/toml
  (:nicknames :toml)
  (:use :cl :std :time :ast)
  (:export
   #:parse-toml-blocks
   :parse-toml-value
   #:toml-collection
   #:toml-table
   #:inline-toml-table
   #:toml-table-array
   #:toml-redefine-table-error
   #:toml-redefine-property-error
   #:toml-modify-inline-table-error
   #:toml-dotted-key-redefine-table-error
   #:toml-dotted-key-open-table-array-error
   #:toml-document))

(defpkg :dat/arff
  (:nicknames :arff)
  (:use :cl :std :dat/csv)
  (:export
   :arff :arff-relation :arff-attributes :arff-data :arff-path 
   :parse-arff :parse-arff-string :parse-arff-stream
   :remove-attribute-by-name))

(defpkg :dat/bib
  (:nicknames :bib)
  (:use :std-lisp :ast :id)
  (:export :bibliography))

(defpkg :dat/json
  (:nicknames :json)
  (:use :cl :std :uri :id)
  (:import-from :ast :ast :load-ast)
  (:export
   #:*allow-json-trailing-commas*
   #:json-trailing-commas-p
   #:json-decode
   #:json-encode
   #:json-read
   #:json-enable-reader-macro
   #:json-object
   #:json-getf
   #:json-setf
   #:json-remf
   #:json-delf
   #:json-write
   #:json-pointer-p #:json-pointer-from-string
   #:json-schema #:json-schema-validate))

(defpkg :dat/openapi
  (:nicknames :openapi)
  (:use :cl :std :dat/json :schema)
  (:import-from :ast :ast))

(defpkg :dat/base64
  (:use :cl :std)
  (:export 
   #:base64-stream-to-integer
   #:base64-stream-to-string
   #:base64-stream-to-stream
   #:base64-stream-to-octet-vector
   #:base64-string-to-integer
   #:base64-string-to-string
   #:base64-string-to-stream
   #:base64-string-to-octet-vector
   #:string-to-base64-string
   #:string-to-base64-stream
   #:octet-vector-to-base64-string
   #:octet-vector-to-base64-stream
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

(defpkg :dat/m3u
  (:nicknames :m3u)
  (:use :cl :std)
  (:export
   :read-m3u-file
   :read-m3u-stream
   :write-m3u-file
   :write-m3u-stream
   :parse-m3u-string
   :write-m3u-string))

(defpkg :dat/id3
  (:use :cl :std :id)
  (:export
   :read-id3
   :mp3-p
   :id3-p
   :decode-u28
   :read-id3-header
   :show-id3-header
   :show-id3-headers))

(defpkg :dat/css
  (:nicknames :css)
  (:use :cl :std :color)
  (:export
   :parse-css
   :*minify-css*
   :parse-css-selector
   :parse-css-fragment
   :px :em :% :ch
   :vw :vh
   :blur
   :inline-css
   :css
   :compile-css
   :compile-css-file))

(defpkg :dat/xml
  (:use :std-lisp)
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
   :write-doctype
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

(defpkg :dat/svg
  (:nicknames :svg)
  (:use :cl :ppcre :dat/xml :std)
  (:import-from :std/string :*whitespaces*)
  (:export :parse-svg-file :parse-svg-string))

(defpkg :dat/html
  (:nicknames :html)
  (:use :cl :std/macs :std/string :std/io :std/condition :std/stream)
  (:import-from :std :with-gensyms)
  (:import-from :sb-ext :defglobal)
  (:import-from :ast :ast)
  (:export
   :with-html
   :with-html-string
   :htm :fmt :esc :str
   :html-output-stream
   :*html-output*
   :*html-lang*
   :*html-charset*
   :*html-indent*
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

(defpkg :dat/mime
  (:use :cl :std :dat/xml)
  (:export :*mime-database*
   :load-mime-info :update-mime-database
   :mime-magic :mime-magic-offset :mime-magic-type :mime-magic-value
   :mime-type :mime-type-name :mime-type-superclasses :mime-type-glob
   :mime-type-glob :mime-type-magic
   :mime :get-mime :get-mime*
   :mime-case))

(defpkg :dat/yaml
  (:use :cl)
  (:import-from :log :trace!))

(defpkg :dat/midi
  (:nicknames :midi)
  (:use :cl :std :io/proto)
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
           #:message-value #:midi-message
           #:unknown-event #:data-byte #:dd #:bb #:cc #:nn))

(defpkg :dat/png
  (:nicknames :png)
  (:use :cl :std :io/proto :io/flate :color)
  (:import-from :io/deflate :zlib-compressor)
  (:export
   :png
   :copy-png
   :png=
   :write-png
   :write-png-stream
   :streamed-png
   :start-png
   :finish-png
   :pixel-streamed-png
   :png-error))

(defpkg :dat/qrcode
  (:nicknames :qrcode)
  (:use :cl :std :dat/png)
  (:export
   :encode-symbol
   ;; QR code representation
   ;; this should be enough to write another backend for QR symbol
   :qr-symbol
   :matrix
   :modules
   :dark-module-p
   :qr-symbol-to-png
   :qr-encode-png
   :qr-encode-png-stream
   :qr-encode-png-bytes
   :qr-encode-png-bytes-stream))

(defpkg :dat/jpeg
  (:use :std-lisp :dat/img :io/proto :color :jpeg)
  (:export :jpeg-image))

(defpkg :dat/gif
  (:nicknames :gif)
  (:use :cl :std :io/proto :io/lzw :color :dat/img :io/flate)
  (:export :make-gif-image :make-gif-stream :gif-error
   :*gif-delay-time* :*gif-disposal-methods*
   :gif-image :gif-stream :interlacedp :interlace :deinterlace :add-delay
   :loopingp :output-gif-stream))

(defpkg :dat/ttf
  (:nicknames :ttf)
  (:use :cl :std :io)
  (:export
   ;; font-cache
   :*font-cache*
   :font
   :font-family
   :font-subfamily
   :font-size
   :font-underline
   :font-strikethrough
   :font-overline
   :font-background
   :font-foreground
   :font-overwrite-gcontext
   :cache-font-file
   :*font-dirs*
   :get-font-families
   :get-font-subfamilies
   :font-antialias
   :cache-fonts
   :font-equal
   ;; font string
   :font-string-alpha-maps
   :font-string-bboxes
   :font-string-line-alpha-maps
   :font-string-line-bboxes
   ;; font loader
   #:open-font-loader
   #:close-font-loader
   #:with-font-loader
   #:with-font
   #:glyph-count
   #:name-entry-value
   #:find-name-entry
   #:collection-font-count
   #:collection-font-index
   ;; font typographic
   #:italic-angle
   #:underline-thickness
   #:underline-position
   #:fixed-pitch-p
   #:units/em
   #:ascender
   #:descender
   #:line-gap
   #:max-width
   #:vascender
   #:vdescender
   ;; other font attributes
   #:postscript-name
   #:full-name
   #:family-name
   #:subfamily-name
   #:all-kerning-pairs
   #:glyph-exists-p
   #:index-glyph
   #:find-glyph
   ;; shared between font-loader and glyph
   #:bounding-box
   #:bbox-xmin
   #:bbox-ymin
   #:bbox-xmax
   #:bbox-ymax
   ;; control points
   #:cp-x
   #:cp-y
   #:on-curve-p
   ;; glyph contours
   #:contour-count
   #:contour
   #:contours
   #:do-contours
   #:explicit-contour-points
   #:do-contour-segments
   #:do-contour-segments*
   ;; glyph other
   #:code-point
   #:font-index
   ;; glyph typographic
   #:advance-width
   #:advance-height
   #:left-side-bearing
   #:top-side-bearing
   #:right-side-bearing
   #:kerning-offset
   #:string-bounding-box
   ;; conditions
   #:regrettable-value
   #:regrettable-hex-value
   #:bad-magic
   #:unsupported-version
   #:unsupported-format
   #:unsupported-value))

(defpkg :dat/tar
  (:nicknames :tar)
  (:use :cl :std 
   :io/proto :io/chunky :sb-ext :io/stream
   :std/stream :std/macs :std/ht :io/flate)
  (:shadow :version)
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
   #:do-entries
   #:write-file-entry
   #:write-hard-link-entry
   #:write-symbolic-link-entry
   #:write-block-device-entry
   #:write-directory-entry
   #:write-fifo-entry
   #:write-pax-extended-attributes-entry
   #:write-pax-global-attributes-entry
   #:write-gnu-long-link-name-entry
   #:write-gnu-long-name-entry))

(defpkg :dat/zip
  (:nicknames :zip)
  (:use :cl :std 
   :io/proto :io/chunky :sb-ext :io/stream
   :std/stream :std/macs :std/ht :io/flate))

(defpkg :dat/parquet
  (:use :cl :std :obj/id :dat/json)
  (:export
   :parquet-object
   :parquet-enum-object
   :parquet-struct-object
   :parquet-read
   :parquet-write
   :parquet-encode
   :parquet-decode))
