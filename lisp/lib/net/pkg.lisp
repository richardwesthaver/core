(defpackage :net/core
  (:use :cl :std :sb-bsd-sockets :sb-thread :sb-concurrency)
  (:export
   ;; err
   :net-error
   :codec-error
   :protocol-error
   ;; obj
   :transport
   :codec
   :protocol
   :client
   :server
   :peer
   :proxy
   :tunnel))

(defpackage :net/util
  (:use :cl :obj :dat/proto :std :log :net/core :sb-bsd-sockets)
  (:export :get-address-by-name))

(defpackage :net/sans-io
  (:use :cl :obj :dat/proto :std :net/core :sb-bsd-sockets)
  (:export :sans-io-protocol :protocol-version :protocol-name :protocol-features
   :*max-connection-id* :*initial-mtu* :*max-stream-count* :*max-udp-payload*
   :*word-length* :sans-io-error :packet-serializer-error :packet-deserializer-error
   :packet-header-serializer-error :packet-header-deserializer-error :frame-serializer-error :frame-deserializer-error
   :stream-id :stream-direction :event-id :event
   :endpoint-event :connection-event :connection-id :connection-id-generator
   :connection :connection-idle-timeout :peer-id :peer-address
   :peer :clientp :serverp :endpoint-config
   :transport-config :server-config :client-config :endpoint
   :handle-event :handle :connect :default-client-config
   :packet-number :packet-header :packet-payload :packet
   :frame :size-bound :frame-type :with-endpoint
   :with-client :define-protocol :define-endpoint :define-event
   :define-handler))

(defpackage :net/udp
  (:nicknames :udp)
  (:use :cl :std :net/core :sb-bsd-sockets)
  (:export
   :udp-server
   :with-udp-client
   :with-udp-server
   :with-udp-client-and-server))

(defpackage :net/tcp
  (:nicknames :tcp)
  (:use :cl :std :net/core :sb-bsd-sockets)
  (:export
   :tcp-server
   :with-tcp-client))

(defpackage :net/codec/punycode
  (:nicknames :codec/punycode)
  (:use :cl)
  (:export
   :encode-punycode
   :decode-punycode
   :encode-domain
   :decode-domain))

(defpackage :net/codec/dns
  (:nicknames :codec/dns)
  (:use :cl :std :net/core :net/codec/punycode)
  (:export
   :*record-type-table*
   :record-type-id
   :id-record-type
   :decode-record :decode-response
   :encode-host :decode-host :encode-header :decode-header
   :encode-query :decode-query
   :decode-data))

(defpackage :net/codec/tlv
  (:nicknames :codec/tlv)
  (:use :cl :std :net/core :dat/proto)
  (:export
   :tlv :tlv-type :tlv-length :tlv-value :make-tlv))

(defpackage :net/codec/osc
  (:nicknames :codec/osc)
  (:use :cl :std :log :net/core)
  (:export
   :*default-osc-buffer-size*
   :make-message
   :message
   :make-bundle
   :bundle
   :format-osc-data
   :command
   ;; :args ;; conflict
   :timetag
   :elements
   :encode-message
   :encode-bundle
   :decode-message
   :decode-bundle
   :make-osc-tree
   :dp-register
   :dp-remove
   :dp-match
   :dispatch
   :get-current-timetag            ; osc-time
   :timetag+
   :get-unix-time
   :unix-time->timetag
   :timetag->unix-time
   :print-as-double))

;; sb-thread::make-condition
;; (defpackage :net/proto/crew
;;   (:nicknames :net/crew)
;;   (:use :cl :sb-bsd-sockets :std :net/core :obj/id)
;;   (:import-from #:sb-thread
;;                 #:condition-notify
;;                 #:condition-wait
;;                 ;; #:make-condition-variable
;;                 #:make-mutex ;; make-lock
;;                 #:make-thread
;;                 #:with-mutex)
;;   (:import-from :sb-concurrency
;;                 :make-gate)
;;   (:import-from #:swank-client
;;                 #:slime-close
;;                 #:slime-connect
;;                 #:slime-eval
;;                 #:slime-eval-async
;;                 #:slime-migrate-evals
;;                 #:slime-network-error
;;                 #:slime-pending-evals-p
;;                 #:swank-connection
;;                 #:with-slime-connection)
;;   (:export 
;;    :crew-connection-info
;;    :make-worker-pool
;;    :crew-worker :crew-worker-pool
;;    :*crew-worker-pools-lock*
;;    :*crew-worker-pools*
;;    :connect-worker
;;    :disconnect-worker
;;    :parallel-mapcar :parallel-reduce
;;    :eval-form-all-workers
;;    :eval-form-repeatedly
;;    :eval-repeatedly-async-state   
;;    :worker-count
;;    :reconnect-worker))

(defpackage :net/proto/swank
  (:use :cl :sb-bsd-sockets :std :net/core :net/tcp)
  (:export))

(defpackage :net/proto/dns
  (:nicknames :net/dns)
  (:use :cl :sb-bsd-sockets :std :net/core :net/udp :codec/dns)
  (:export
   :dns-port
   :*cloudflare-servers*
   :*dnswatch-servers*
   :*google-servers*
   :*opendns-servers*
   :*quad9-servers*
   :*dns-servers*
   :query
   :query-data
   :resolve
   :hostname
   :response-code
   :dns-servers-exhausted
   :response-code-name
   :with-dns-error-handling))

(defpackage :net/proto/ssh
  (:use :cl :std :net/core :sb-bsd-sockets)
  (:export))

(defpackage :net/proto/http
  (:nicknames :http)
  (:use :cl :std :net/core :sb-bsd-sockets :parse/bytes :io/xsubseq :io/smart-buffer)
  (:export
   :make-http-parser
   :http-request
   :http-response
   :make-http-request
   :make-http-response
   :http-request-p
   :http-response-p
   :make-callbacks
   :http-version
   :http-major-version
   :http-minor-version
   :http-method
   :http-resource
   :http-status
   :http-status-text
   :http-content-length
   :http-chunked-p
   :http-upgrade-p
   :http-headers
   ;; multipart parser
   :make-multipart-parser
   ;; Low-level parser API
   :http
   :http-p
   :make-http
   :parse-request
   :parse-response
   :http-multipart-parse
   :ll-multipart-parser
   :make-ll-multipart-parser
   ;; Error
   :http-error
   :callback-error
   :cb-message-begin
   :cb-url
   :cb-first-line
   :cb-header-field
   :cb-header-value
   :cb-headers-complete
   :cb-body
   :cb-message-complete
   :cb-status

   :parsing-error
   :invalid-eof-state
   :header-overflow
   :closed-connection
   :invalid-version
   :invalid-status
   :invalid-method
   :invalid-url
   :invalid-host
   :invalid-port
   :invalid-path
   :invalid-query-string
   :invalid-fragment
   :lf-expected
   :invalid-header-token
   :invalid-content-length
   :invalid-chunk-size
   :invalid-constant
   :invalid-internal-state
   :strict-error
   :paused-error
   :unknown-error

   :multipart-parsing-error
   :invalid-multipart-body
   :invalid-boundary

   :header-value-parsing-error
   :invalid-header-value
   :invalid-parameter-key
   :invalid-parameter-value))

(defpackage net/cookie
  (:use :cl :std :parse/bytes :obj/uri)
  (:shadowing-import-from :alexandria :when-let :if-let)
  (:import-from :obj/time
   :today
                :timestamp-century
   :timestamp-to-universal
                :universal-to-timestamp
   :format-timestring
                :encode-timestamp
   :*abbreviated-subzone-name->timezone-list*
                :reread-timezone-repository
   :timezone-subzones
                :subzone-abbrev
   :subzone-offset
                :+gmt-zone+)
  (:export :parse-set-cookie-header
   :write-cookie-header
           :write-set-cookie-header
   :cookie
           :make-cookie
   :cookie=
           :cookie-equal
   :cookie-name
           :cookie-value
   :cookie-expires
           :cookie-path
   :cookie-domain
           :cookie-same-site
   :cookie-max-age
           :cookie-partitioned
   :cookie-secure-p
           :cookie-httponly-p
   :cookie-origin-host
           :cookie-jar
   :make-cookie-jar
           :cookie-jar-cookies
   :cookie-jar-host-cookies
           :merge-cookies))

(defpackage :net/req
  (:nicknames :req)
  (:shadowing-import-from :std/type :octet :octet-vector)
  (:shadow :get :delete)
  (:use :cl :std :obj/uri :net/proto/http :babel :net/cookie :fast-io :dat/base64 :cl+ssl :sb-gray)
  (:shadowing-import-from :babel :octets-to-string)
  (:export
   :request
   :get
   :post
   :head
   :put
   :patch
   :delete
   :fetch
   :*default-connect-timeout*
   :*default-read-timeout*
   :*default-proxy*
   :*verbose*
   :*no-ssl*
   :*connection-pool*
   :*use-connection-pool*
   :make-connection-pool
   :clear-connection-pool
   ;; Restarts
   :retry-request
   :ignore-and-continue))

(defpackage :net/fetch
  (:nicknames :fetch)
  (:use :cl :std :obj/uri)
  (:export :fetch :download))

(defpackage :net/srv
  (:nicknames :srv)
  (:use :cl :std :obj/uri :net/core :net/proto/http :net/sans-io :net/cookie :dat/base64 :sb-gray)
  (:export))

(in-package :std-user)

(defpkg :net
  (:use-reexport 
   :net/core 
   :net/tcp 
   :net/udp
   :net/sans-io
   :net/codec/dns 
   :net/codec/osc 
   :net/codec/tlv
   :net/proto/dns 
   ;; :net/proto/crew 
   :net/proto/ssh
   :net/proto/http))
