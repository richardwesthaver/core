;;; pkg.lisp --- Network Packages

;; 

;;; Code:
(defpkg :net/core
  (:use :cl :std :sb-thread :config :id :io/socket :io/mux)
  (:use-reexport :sb-bsd-sockets)
  (:recycle :sb-bsd-sockets)
  (:export
   ;; sb-bsd-sockets
   :family
   :size-of-sockaddr
   :make-sockaddr-for
   :bits-of-sockaddr
   :free-sockaddr-for
   ;; obj
   :*ipv6*
   :net-condition
   :net-error
   :net-warning
   :net-config
   :socket-config
   :socket-peername
   :socket-element-type
   :socket-address
   :socket-error
   :socket-name
   :socket-accept
   :socket-bind
   :socket-family
   :socket-close
   :socket-send
   :socket-receive
   :socket-shutdown
   :socket-connect
   :socket-list
   :socket-protocol
   :socket-open-p
   :socket-make-stream
   :socket-file-descriptor
   :ip-address
   :socket
   :port
   :privileged-port :unprivileged-port
   :client
   :client-config
   :server
   :server-config
   :peer
   :router
   :endpoint
   :route
   :proxy
   :connection
   :connect
   :disconnect
   :with-open-connection
   :send-message
   :connection-fd
   :connection-pending-messages
   :connection-next-serial
   :drain-pending-messages
   :wait-for-reply
   :receive-message-no-hang
   :connection-server-id
   :connection-server-address
   :make-client
   :make-server
   :make-client-request
   :make-server-response
   :default-inet-address-family)
  ;; utils
  (:export :get-address-by-name
   :with-client-server :*localhost*
   :with-open-socket :find-port
   :*wildcard-host* :*wildcard-port*
   :*default-mtu*
   :*default-connect-timeout*
   :*default-read-timeout*
   :*default-proxy*
   :*default-user-agent*)
  ;; udp
  (:export
   :udp-server
   :with-udp-client
   :with-udp-server
   :udp-receive-ping
   :udp-echo
   :udp-socket
   :udp-client)
  ;; tcp
  (:export
   :tcp-server
   :with-tcp-client
   :tcp-ping-server
   :*tcp-ping-size*
   :tcp-echo
   :tcp-receive-ping
   :tcp-client
   :tcp-source
   :tcp-sink
   :tcp-socket
   :tcp-config)
  ;; unix
  (:export
   :unix-server
   :with-unix-client
   :with-unix-server
   :unix-receive-ping
   :unix-echo
   :unix-socket
   :unix-client)
  ;; netlink
  (:export 
   :netlink-socket)
  ;; socket
  (:export
   :make-socket))

(defpkg :net/codec/dns
  (:nicknames :codec/dns)
  (:use :cl :std :net/core :punycode)
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

(defpkg :net/codec/tlv
  (:nicknames :codec/tlv :net/tlv :tlv)
  (:use :cl :std :net/core)
  (:export
   :tlv :tlv-type :tlv-length :tlv-value :make-tlv))

(defpkg :net/codec/osc
  (:nicknames :codec/osc)
  (:use :cl :std :log :net/core)
  (:import-from :obj/time :get-unix-time)
  (:export
   :*default-osc-buffer-size*
   :make-osc-message
   :osc-message
   :make-osc-bundle
   :osc-bundle
   :format-osc-data
   :osc-command
   :osc-args
   :osc-timetag
   :osc-elements
   :decode-osc-message
   :decode-osc-bundle
   :make-osc-tree
   :osc-register
   :osc-remove
   :osc-match
   :osc-dispatch))

(defpkg :net/codec/http
  (:use :cl :net/core)
  (:import-from :std :eval-always :define-constant
   :hash-table-alist)
  (:export :+known-http-words+ 
   :*http-status-message-map* :http-status-message :http-keyword :+known-http-versions+
   :+known-http-methods+))

(defpkg :net/codec/dbus
  (:use :std-lisp :net/core :dat/xml)
  (:import-from :sb-ext :string-to-octets :octets-to-string)
  (:export 
   :encode-dbus-message :decode-dbus-message
   :dbus-message :dbus-type
   :dbus-type-table :find-dbus-type
   :define-dbus-type :*dbus-type-table*
   :invoke-method :+message-no-reply-expected+
   :+message-no-auto-start+
   :dbus-standard-message :dbus-method-call-message
   :dbus-signal-message :dbus-method-return-message
   :dbus-error-message :dbus-object
   :dbus-object-handler-lookup-table
   :find-dbus-object :register-dbus-object
   :define-dbus-object :introspection-document
   :output-introspection-fragment :define-dbus-signal-handler
   :register-dbus-signal-handler :dbus-signal-handler
   :*all-dbus-objects* :*authenticator-classes* 
   :find-authenticator-class :dbus-handler :dbus-signal-handler :dbus-method-handler))

(defpkg :net/proto/whois
  (:nicknames :net/whois)
  (:use :cl :std :net/core :punycode))

(defpkg :net/proto/dict
  (:nicknames :net/dict)
  (:use :cl :std :net/core))

(defpkg :net/proto/dns
  (:nicknames :net/dns)
  (:use :cl :std :net/core :codec/dns)
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

(defpkg :net/proto/dbus
  (:use :std-lisp :net/core :cry/auth :net/codec/dbus :cry/keyring :io/mux)
  (:import-from :net/codec/dbus
   :message-endianness :message-flags :message-major-protocol-version :message-body-length
   :message-serial :message-sender :message-signature :message-body
   :message-member :message-reply-serial :message-error-name :message-interface
   :signature :valid-body-p :handler-output-signature :handler-function
   :handler-full-lisp-name :handler-input-signature :full-member-name :require-dbus-object)
  (:export :dbus-error :dbus-auth-error :dbus-method-error))

(defpkg :net/proto/ssh
  (:use :std-lisp :net/core)
  (:export))

(defpkg :net/proto/http
  (:nicknames :http)
  (:use-reexport :net/codec/http)
  (:use :cl :std :net/core :parse/bytes :io/xsubseq :io/smart-buffer :config)
  (:export
   :http-config
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

(defpkg :net/proto/transmission
  (:nicknames :net/transmission)
  (:import-from :id :id)
  (:import-from :uri :uri)
  (:import-from :srv :request :response :service :session :request-protocol :content-stream)
  (:use :cl :std :net/core :net/proto/http))

(defpkg :net/proto/dm
  (:nicknames :net/dm)
  (:use :cl :std :net/core :net/codec/tlv))

(defpkg :net/proto/sesh
  (:nicknames :net/sesh)
  (:use :std-lisp :net/core))

(defpkg :net/cookie
  (:use :cl :std :parse/bytes :obj/uri)
  (:shadowing-import-from :std :when-let :if-let)
  (:import-from :obj/time
   :today :timestamp-century
   :timestamp-to-universal :universal-to-timestamp
   :format-timestring :encode-timestamp
   :*abbreviated-subzone-name->timezone-list* :reread-timezone-repository
   :timezone-subzones :subzone-abbrev
   :subzone-offset :+gmt-zone+)
  (:export :parse-set-cookie-header
   :write-cookie-header :write-set-cookie-header
   :cookie :make-cookie
   :cookie= :cookie-equal
   :cookie-name :cookie-value
   :cookie-expires :cookie-path
   :cookie-domain :cookie-same-site
   :cookie-max-age :cookie-partitioned
   :cookie-secure-p :cookie-httponly-p
   :cookie-origin-host :cookie-jar
   :make-cookie-jar :cookie-jar-cookies
   :cookie-jar-host-cookies :merge-cookies
   :cookie-p :copy-cookie
   :cookie-creation-timestamp :stringify-cookie
   :cookie-date))

(defpkg :net/proto/swank
  (:nicknames :net/swank)
  (:use :std-lisp :net/core)
  (:export 
   #:*swank-connections*
   #:*default-swank-port*
   #:swank-connection
   #:slime-connect
   #:slime-close
   #:slime-eval
   #:slime-eval-async
   #:slime-migrate-evals
   #:slime-network-error
   #:slime-pending-evals-p
   #:with-slime-connection
   #:slime-connect-file))

(defpkg :net/proto/crew
  (:use :std-lisp :net/core :obj/id :net/proto/swank)
  (:import-from #:sb-thread
                #:condition-notify
                #:condition-wait
                #:make-mutex
                #:make-thread
                #:with-mutex)
  (:import-from :sb-concurrency :make-gate)
  (:export 
   :crew-connection-info
   :make-crew
   :crew-worker :crew-worker-pool
   :connect-worker
   :disconnect-worker
   :parallel-mapcar :parallel-reduce
   :eval-form-all-workers
   :eval-form-repeatedly
   :eval-repeatedly-async-state   
   :reconnect-worker))

(defpkg :net/req
  (:nicknames :req)
  (:shadowing-import-from :std/type :octet :octet-vector)
  (:import-from :dat/mime :mime)
  (:import-from :chunky :input-chunking-p :make-chunked-stream :output-chunking-p)
  (:import-from :io/fast :make-output-buffer :finish-output-buffer)
  (:import-from :io/stream :decoding-stream :needs-to-fill-buffer-p
   :+buffer-size+ :make-decoding-stream :dec-fill-buffer)
  (:import-from :log :trace-p)
  (:import-from :obj/srv :send-request)
  (:shadow :get :delete :head)
  (:import-from :sb-ext :string-to-octets)
  (:use :cl :std :uri
   :url :http :net/cookie :net/core
   :io/fast :io/chunky
   :dat/base64 :ssl :sb-gray :config)
  (:export
   :request
   :get
   :post
   :head
   :put
   :patch
   :delete
   :fetch
   :http-client
   :*verbose*
   :+socks5-version+
   :*connection-pool*
   :*use-connection-pool*
   :make-connection-pool
   :clear-connection-pool
   ;; Restarts
   :retry-request
   :ignore-and-continue
   :decoding-stream-of))

(defpkg :net/srv
  (:use :cl :obj/uri :log
   :net/core :net/proto/http :net/cookie :dat/base64
   :sb-gray :dat/mime :obj/db :obj/schema :config :build :srv :ast :std/thread)
  (:import-from :chunky :chunked-stream :input-chunking-p :output-chunking-p)
  (:import-from :std :defvar-unbound :once-only 
   :deferror :defwarning :with-gensyms :deserialize
   :eval-always :when-let :stream-of :symbolicate :defaccessor :data)
  (:import-from :rt :random-chars)
  (:import-from :sb-thread :make-mutex :with-mutex)
  (:import-from :std/thread :shutdown :start :stop :started-p)
  (:export
   :service-make-request
   :with-request-count-incf
   :shutdown-p
   #:+handler-tag+
   #:process-request
   #:default-web-directory
   #:add-route
   #:delete-route
   #:*router*
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
   #:*session-secret*
   #:*service-stream*
   #:*finish-processing-socket*
   #:*close-service-stream*
   #:find-route
   #:next-session-id
   #:remove-session
   #:session-expired-p
   #:defroute
   #:defservice
   :service-log
   #:*headers-sent*
   #:*log-service-errors*
   #:net-service-config
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
   #:call-with-request-count-incf
   #:detach-socket
   #:session-timeout
   #:service-timeout
   #:net-response
   #:net-request
   #:net-service
   #:*access-log-lock*
   #:*message-log-lock*
   #:make-service
   #:net-service-response
   #:net-service-request
   #:abort-request-handler
   #:net-service-config))

(defpkg :net/srv/http
  (:use :cl :std :net/proto/http
   :net/codec/http :net/core :net/cookie :io/chunky 
   :srv :config)
  (:import-from :net/srv :service-log)
  (:import-from :io/chunky :trim-whitespace)
  (:use-reexport :net/srv)
  (:package-local-nicknames
   :codec :net/codec/http
   :proto :net/proto/http)
  (:export :http-service :https-service :http-service-config))

(defpkg :net/srv/udp
  (:use :cl :std :net/codec/tlv :net/core :srv :config)
  (:use-reexport :net/srv)
  (:export :udp-service :echo-service :udp-service-config))

(defpkg :net/srv/oauth
  (:use :cl :std :net/codec/http :net/cookie :net/core :id :secret :uri :net/srv/http :srv :config)
  (:import-from :cli/tools/net :browse-url)
  (:use-reexport :net/srv)
  (:export :oauth-service :oauth-service-config))

(defpkg :net/srv/openapi
  (:use :cl :std :net/proto/http :net/core :id :secret :uri :net/srv/http :srv :dat/json :ast :config)
  (:import-from :net/req :http-client :http-client-config)
  (:use-reexport :net/srv)
  (:export :openapi-service :openapi-document :oapi-client :oapi-server :openapi-service-config))

(defpkg :net/srv/ext
  (:use :cl :std :net/core :cli/tools/net :config)
  (:export :caddy-service :nginx-service))

(setq *defpkg-hook* nil)

(defpkg :net
  (:use :cl :std)
  (:import-from :net/req :http-client-config :http-client)
  (:export :http-client-config :http-client))

(export-packages (remove "NET/REQ" *component-packages* :test 'string=) :net)

(defpkg :net-user (:use :cl :std :net :uri :url))
