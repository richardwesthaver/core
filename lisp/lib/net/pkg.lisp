(defpackage :net/core
  (:use :cl :std :sb-bsd-sockets :sb-thread :sb-concurrency)
  (:export
   ;; err
   :net-error
   :codec-error
   :protocol-error
   ;; obj
   :connection
   :transport
   :codec
   :protocol
   :client
   :server
   :peer
   :proxy
   :tunnel))
  
(defpackage :net/udp
  (:nicknames :udp)
  (:use :cl :std :net/core :sb-bsd-sockets)
  (:export
   :udp-server
   :with-udp-client-and-server))

(defpackage :net/tcp
  (:nicknames :tcp)
  (:use :cl :std :net/core :sb-bsd-sockets)
  (:export
   :tcp-server))

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
  (:use :cl :std :net/core)
  (:export
   :*record-type-table*
   :record-type-id
   :id-record-type))

(defpackage :net/codec/tlv
  (:nicknames :codec/tlv)
  (:use :cl :std :net/core)
  (:export
   :tlv :make-tlv))

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
   :args
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
(defpackage :net/proto/crew
  (:nicknames :net/crew)
  (:use :cl :sb-bsd-sockets :std :net/core)
  (:import-from #:sb-thread
                #:condition-notify
                #:condition-wait
                ;; #:make-condition-variable
                #:make-mutex ;; make-lock
                #:make-thread
                #:with-mutex)
  (:import-from :sb-concurrency
                :make-gate)
  (:import-from #:swank-client
                #:slime-close
                #:slime-connect
                #:slime-eval
                #:slime-eval-async
                #:slime-migrate-evals
                #:slime-network-error
                #:slime-pending-evals-p
                #:swank-connection
                #:with-slime-connection)
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

(defpackage :net/fetch
  (:nicknames :fetch)
  (:use :cl :std :drakma)
  (:export :fetch :download))

(uiop:define-package :net
  (:use-reexport :net/core :net/dns :net/crew :net/tcp :net/udp :codec/dns :codec/osc :codec/tlv))
