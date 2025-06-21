;;; pkg.lisp --- Network Packages

;; 

;;; Code:
(defpackage :net/int
  (:use :cl :std)
  (:export :*net-packages*))
(in-package :net/int)

(eval-always (defparameter *net-packages* nil))

(setq *defpkg-hook* (compile nil (lambda (x) (pushnew (package-name x) *net-packages* :test 'string=))))

(defpkg :net/core
  (:use :cl :std :sb-thread :config :id)
  (:recycle :sb-bsd-sockets)
  (:export
   ;; obj
   :net-condition
   :protocol-condition
   :codec-condition
   :net-error
   :codec-error
   :protocol-error
   :transport
   :codec
   :protocol
   :net-config
   :client
   :client-config
   :server
   :server-config
   :peer
   :proxy
   :tunnel
   :codec-warning
   :protocol-warning
   :connect
   :disconnect
   :make-client
   :make-server)
  ;; utils
  (:export :get-address-by-name
   :with-client-server :*localhost*)
  ;; pkg
  (:export :*net-packages*))

(defpkg :net/udp
  (:nicknames :udp)
  (:use :cl :std :net/core :sb-bsd-sockets :config)
  (:export
   :udp-server
   :with-udp-client
   :with-udp-server
   :udp-receive-ping
   :udp-echo
   :udp-socket
   :udp-client))

(defpkg :net/tcp
  (:nicknames :tcp)
  (:use :cl :std :net/core :sb-bsd-sockets :config)
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
   :tcp-config))

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
  (:nicknames :codec/tlv)
  (:use :cl :std :net/core :dat/proto)
  (:export
   :tlv :tlv-type :tlv-length :tlv-value :make-tlv))

(defpkg :net/codec/osc
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

(defpkg :net/codec/http
  (:use :cl :net/core)
  (:import-from :std :eval-always :define-constant
   :hash-table-alist)
  (:export :+known-http-words+ 
   :*http-status-message-map* :http-status-message :http-keyword :+known-http-versions+
   :+known-http-methods+))

(defpkg :net/proto/whois
  (:nicknames :net/whois)
  (:use :cl :sb-bsd-sockets :std :net/core :net/tcp :punycode))

(defpkg :net/proto/dns
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

(defpkg :net/proto/ssh
  (:use :cl :std :net/core :sb-bsd-sockets)
  (:export))

(defpkg :net/proto/http
  (:nicknames :http)
  (:use-reexport :net/codec/http)
  (:use :cl :std :net/core :sb-bsd-sockets :parse/bytes :io/xsubseq :io/smart-buffer :config :net/tcp)
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
  (:use :cl :sb-bsd-sockets :std :net/core :net/tcp :net/proto/http))

(defpkg :net/proto/dm
  (:nicknames :net/dm)
  (:use :cl :sb-bsd-sockets :std :net/core :net/udp :net/codec/tlv))

(defpkg :net/proto/sesh
  (:nicknames :net/sesh)
  (:use :cl :sb-bsd-sockets :std :net/core :net/udp))

(defpkg :net/proto/nsm
  (:nicknames :net/nsm)
  (:use :cl :sb-bsd-sockets :std :net/core :net/udp :codec/osc))

(defpkg net/cookie
  (:use :cl :std :parse/bytes :obj/uri)
  (:shadowing-import-from :alexandria :when-let :if-let)
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

(defpkg :net/req
  (:nicknames :req)
  (:shadowing-import-from :std/type :octet :octet-vector)
  (:import-from :dat/mime :mime)
  (:import-from :chunky :input-chunking-p :make-chunked-stream :output-chunking-p)
  (:import-from :io/fast :make-output-buffer :finish-output-buffer)
  (:shadow :get :delete)
  (:import-from :sb-ext :string-to-octets)
  (:use :cl :std :uri
   :url :http :net/cookie :net/core
   :io/fast :io/chunky
   :dat/base64 :ssl :sb-gray :config
   :net/tcp)
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

(defpkg :net/srv
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
   #:make-service
   #:net-service-response
   #:net-service-request
   #:abort-request-handler
   #:net-service-config))

(defpkg :net/srv/http
  (:use :cl :std :net/proto/http
   :net/codec/http :net/core :net/cookie :io/chunky 
   :srv :net/tcp :config)
  (:import-from :net/srv :service-log)
  (:use-reexport :net/srv)
  (:package-local-nicknames
   :codec :net/codec/http
   :proto :net/proto/http)
  (:export :http-service :https-service :http-server-config :https-server-config :tls-config))

(defpkg :net/srv/udp
  (:use :cl :std :net/udp :net/codec/tlv :net/core :srv)
  (:use-reexport :net/srv)
  (:export :udp-service :echo-service))

(defpkg :net/srv/oauth
  (:use :cl :std :net/codec/http :net/cookie :net/core :id :secret :uri :net/srv/http :srv)
  (:import-from :cli/tools/net :browse-url)
  (:use-reexport :net/srv)
  (:export :oauth-service))

(defpkg :net/srv/openapi
  (:use :cl :std :net/proto/http :net/core :id :secret :uri :net/srv/http :srv :dat/json)
  (:use-reexport :net/srv)
  (:export :openapi-service))

(defpkg :net/srv/ext
  (:use :cl :std :net/core :cli/tools/net)
  (:export :caddy-service :nginx-service))

(setq *defpkg-hook* nil)

(eval-always
  (defpkg :net
    (:use :cl :std)
    #.`(:use-reexport ,@(remove "NET/REQ" net/int:*net-packages* :test 'string=))
    (:import-from :net/req :http-client-config :http-client)
    (:export :http-client-config :http-client)))

(defpkg :net-user
  (:use :cl :std :net :uri :url))

(in-package :net)
(when (sb-int:featurep :swank)
  #+quicklisp (ql:quickload '(:swank :swank-client))
  (load (asdf:system-relative-pathname :net "proto/swank.lisp"))
  (load (asdf:system-relative-pathname :net "proto/crew.lisp"))
  (use-package :net/proto/swank)
  (use-package :net/proto/crew))

(eval-when (:load-toplevel)
  (pushnew :net *features*))
