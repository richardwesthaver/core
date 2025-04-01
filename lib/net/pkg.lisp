;;; pkg.lisp --- Network Packages

;; 

;;; Code:
(pushnew :net *features*)

(pkg:defpkg :net/core
  (:use :cl :std :sb-thread :sb-concurrency)
  (:recycle :sb-bsd-sockets)
  (:export
   ;; err
   :net-condition
   :protocol-condition
   :codec-condition
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
   :tunnel
   :codec-warning
   :protocol-warning
   :connect
   :disconnect
   :make-client
   :make-server))

(defpackage :net/util
  (:use :cl :obj :dat/proto :std :log :net/core :sb-bsd-sockets)
  (:export :get-address-by-name
   :with-client-server))

(defpackage :net/udp
  (:nicknames :udp)
  (:use :cl :std :net/core :sb-bsd-sockets)
  (:export
   :udp-server
   :with-udp-client
   :with-udp-server
   :udp-receive-ping
   :udp-echo
   :udp-socket
   :udp-client))

(defpackage :net/tcp
  (:nicknames :tcp)
  (:use :cl :std :net/core :sb-bsd-sockets)
  (:export
   :tcp-server
   :with-tcp-client
   :tcp-ping-server
   :*tcp-ping-size*
   :tcp-echo
   :tcp-receive-ping))

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
   :dns-condition
   :dns-server-failure
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
   :make-osc-message
   :osc-message
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
   ;; :dispatch
   :get-current-timetag            ; osc-time
   :timetag+
   :get-unix-time
   :unix-time->timetag
   :timetag->unix-time
   :print-as-double))

(defpackage :net/codec/http
  (:use :cl :net/core)
  (:import-from :std :eval-always :define-constant
   :hash-table-alist)
  (:export :+known-http-words+ 
   :*http-status-message-map* :http-status-message :http-keyword :+known-http-versions+
   :+known-http-methods+))

(defpackage :net/proto/whois
  (:nicknames :net/whois)
  (:use :cl :sb-bsd-sockets :std :net/core :net/tcp :codec/punycode))

(defpackage :net/proto/dns
  (:nicknames :net/dns)
  (:use :cl :sb-bsd-sockets :std :net/core :net/udp :codec/dns)
  (:export
   :dns-servers-exhausted
   :dns-port
   :*cloudflare-servers*
   :*dnswatch-servers*
   :*google-servers*
   :*opendns-servers*
   :*quad9-servers*
   :*dns-servers*
   :dns-query
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

(defpackage :net/proto/dm
  (:nicknames :net/dm)
  (:use :cl :sb-bsd-sockets :std :net/core :net/udp :net/codec/tlv))

(defpackage :net/proto/sesh
  (:nicknames :net/sesh)
  (:use :cl :sb-bsd-sockets :std :net/core :net/udp))

(defpackage :net/proto/nsm
  (:nicknames :net/nsm)
  (:use :cl :sb-bsd-sockets :std :net/core :net/udp :codec/osc))

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
           :merge-cookies
   :cookie-p
           :copy-cookie
   :cookie-creation-timestamp
           :stringify-cookie
   :cookie-date))

(defpackage :net/req
  (:nicknames :req)
  (:shadowing-import-from :std/type :octet :octet-vector)
  (:import-from :dat/mime :mime)
  (:import-from :chunky :input-chunking-p :make-chunked-stream :output-chunking-p)
  (:import-from :io/fast :make-output-buffer :finish-output-buffer)
  (:shadow :get :delete)
  (:use :cl :std :obj/uri
   :obj/url :net/proto/http :babel :net/cookie
   :io/fast :io/chunky
   :dat/base64 :ssl :sb-gray)
  (:shadowing-import-from :babel :octets-to-string)
  (:export
   :request
   :get
   :post
   :head
   :put
   :patch
   :delete
   :*default-connect-timeout*
   :*default-read-timeout*
   :*default-proxy*
   :*verbose*
   :*no-ssl*
   :*ca-bundle*
   :+socks5-version+
   :*connection-pool*
   :*use-connection-pool*
   :make-connection-pool
   :clear-connection-pool
   ;; Restarts
   :retry-request
   :ignore-and-continue
   :decoding-stream-of
   :fetch))

(defpackage :net/srv
  (:use :cl :obj/uri :log
   :net/core :net/proto/http :net/cookie :dat/base64
   :sb-gray :dat/mime :sb-bsd-sockets :obj/db 
   :obj/schema :config :build :srv)
  (:import-from :chunky :chunked-stream :input-chunking-p :output-chunking-p)
  (:import-from :std :defvar-unbound :once-only 
   :deferror :defwarning :define-task-kernel :with-gensyms
   :eval-always :define-task-kernel :when-let :stream-of
   :symbolicate :defaccessor :data)
  (:import-from :rt :random-chars)
  (:import-from :sb-thread :make-mutex :with-mutex)
  (:import-from :std/thread :shutdown :start :stop :started-p)
  (:export
   :service-make-request
   :with-request-count-incf
   :shutdown-p
   #:default-web-directory
   #:add-route
   #:delete-route
   #:*router*
   #:*handlers*
   #:http-service-response
   #:accept
   #:handle-connection
   #:initialize-connection-hook
   #:reset-connection-stream
   #:process-connection
   #:secure-service-p
   #:service-log-message
   #:service-log-access
   #:execute-service
   #:service-status-message
   #:start-listening
   #:*default-session-timeout*
   #:*default-ssl-service-port*
   #:*default-max-accept-count*
   #:*default-max-thread-count*
   #:*default-service-port*
   #:*default-connection-max*
   #:*default-connection-timeout*
   #:*global-session-db-lock*
   #:*session-db*
   #:*session*
   #:*services*
   #:*session-secret*
   #:*service-stream*
   #:*finish-processing-socket*
   #:*close-service-stream*
   #:find-route
   #:next-session-id
   #:remove-session
   #:session
   #:session-expired-p
   #:defroute
   #:defservice
   :service-log
   #:*headers-sent*
   #:*log-service-errors*
   #:service-config
   #:session-db
   #:remote-addr*
   #:remote-port*
   #:local-addr*
   #:local-port*
   #:request-protocol*
   #:with-session-db-lock
   #:remove-session-hook
   #:session-database
   #:reset-session-secret
   #:session-value
   #:delete-session-value
   #:session-created
   #:*session-gc-frequency*
   #:session-gc
   #:start-session
   #:session-verify
   #:reset-sessions
   #:service-logger
   #:single-threaded-engine
   #:multi-threaded-engine
   #:thread-per-connection-engine
   #:initialize-instance
   #:increment-accept-count
   #:decrement-accept-count
   #:increment-thread-count
   #:decrement-thread-count
   #:wait-for-free-connection
   #:%handle-connection
   #:create-request-worker-thread
   #:too-many-engine-requests
   #:send-service-unavailable-response
   #:message-log-output
   #:access-log-output
   #:with-open-socket
   #:wake-service-for-shutdown
   #:do-with-request-count-incf
   #:detach-socket
   #:session-timeout
   #:service-timeout
   #:net-response
   #:net-request
   #:net-service
   #:*access-log-lock*
   #:*message-log-lock*
   #:make-service))
