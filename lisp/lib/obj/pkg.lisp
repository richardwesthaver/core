(defpackage :obj/hash
  (:nicknames :hash)
  (:use :cl :std)
  (:export 
   :hash-object))

(defpackage :obj/id
  (:nicknames :id)
  (:use :cl :std :obj/hash)
  (:export 
   :id :id-of :reset-id :update-id :make-id))

(defpackage :obj/seq
  (:nicknames :seq)
  (:use :cl :std)
  (:export :iterator :ring))

(defpackage :obj/tree
  (:nicknames :tree)
  (:use :cl :std :obj/id :obj/seq)
  (:export :node :binary-node :unary-node))

(defpackage :obj/graph
  (:nicknames :graph)
  (:use :cl :std :obj/id :obj/seq :obj/tree)
  (:export 
   :vertex :edge :graph
   :weighted-edge :directed-edge :undirected-edge))

(defpackage :obj/color
  (:nicknames :color)
  (:use :cl :std)
  (:export
   #:rgb #:rgb-red #:rgb-green #:rgb-blue #:gray #:&rgb
   #:hsv #:hsv-hue #:hsv-saturation #:hsv-value #:&hsv
   #:rgb-to-hsv #:hsv-to-rgb #:hex-to-rgb #:as-hsv #:as-rgb
   #:rgb-combination #:hsv-combination
   #:parse-hex-rgb #:print-hex-rgb))

(defpackage :obj/tbl
  (:nicknames :tbl)
  (:use :cl :std)
  (:import-from :uiop :split-string)
  (:export 
   :make-table
   :make-row
   :add-to-table
   :add-to-row
   :get-row
   :get-row-column
   :set-row-column
   :num-rows
   :num-cols
   :num-col
   :rectangular-table-p
   :sequence->row
   :row-sequence->table
   :with-rows
   :select
   :distinct
   :top
   :order-by
   :where
   :where-filter
   :where-or
   :where-and
   :read-csv
   :read-tsv
   :table-from-file))

(defpackage :obj/uri
  ;;  (:nicknames :uri)
  (:use :cl :std :cl-ppcre)
  (:export
   #:uri				; class
   #:uri-p
   #:iri				; subclass of uri
   #:iri-p
   #:copy-uri
   
   #:uri-parse-error
   #:uri-parse-error-string

   #:uri-scheme
   #:uri-userinfo
   #:uri-port
   #:uri-path
   #:uri-query
   #:uri-fragment
   #:generic-uri-scheme
   #:generic-uri-userinfo
   #:generic-uri-port
   #:generic-uri-path
   #:generic-uri-query
   #:generic-uri-fragment

   #:uri-host
   #:uri-ipv6
   #:uri-zone-id
   #:uri-plist
   #:uri-authority			; pseudo-slot accessor

   #:urn				; class
   #:urn-nid
   #:urn-nss
   #:urn-q-component			; RFC 8141
   #:urn-f-component			; RFC 8141
   #:urn-r-component			; RFC 8141
   
   #:*strict-parse*
   #:parse-uri
   #:merge-uris
   #:enough-uri
   #:uri-parsed-path
   #:render-uri
   #:string-to-uri
   #:uri-to-string
   #:string-to-iri
   #:iri-to-string
   #:parse-uri-string-rfc3986
   #:parse-iri-string-rfc3987
   
   #:make-uri-space			; interning...
   #:uri-space
   #:uri=
   #:intern-uri
   #:unintern-uri
   #:do-all-uris
   
   #:uri-to-pathname
   #:pathname-to-uri))

(uiop:define-package :obj
  (:use :cl :std)
  (:use-reexport :obj/color :obj/tbl :obj/id :obj/uri))
