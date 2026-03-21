;;; net/tcp.lisp --- Network Objects

;;

;;; Code:
(in-package :net/core)

;;; Vars
(defvar *ipv6* nil
  "When non-nil, automatically defer to ipv6 addresses where possible.")

(defvar *default-inet-protocol* :tcp)
(defvar *socket-auto-close* t
  "When non-nil arrange for WITH-OPEN-SOCKET to auto-close the socket it opens.")

(defvar *localhost* #(127 0 0 1))
(defvar *wildcard-host* #(0 0 0 0))
(defvar *wildcard-port* 0)
(defparameter *default-user-agent*
  (format nil "req (~A~@[ ~A~]); ~A;~@[ ~A~]"
          (lisp-implementation-type)
          (lisp-implementation-version)
          (software-type)
          (software-version)))
(defvar *default-connect-timeout* 10)
(defvar *default-read-timeout* 10)
(defvar *default-proxy* nil
  "If specified will be used as the default value of PROXY in calls to REQ.")
(defvar *default-mtu* 65507
  "Theoretical maximum bytes in a UDP datagram.

The IPv4 UDP packets have a 16-bit length constraint, and IP+UDP header has
28-byte.

IP_MAXPACKET = 65535,       /* netinet/ip.h */
sizeof(struct ip) = 20,     /* netinet/ip.h */
sizeof(struct udphdr) = 8,  /* netinet/udp.h */

65535 - 20 - 8 = 65507

(But for UDP broadcast, the maximum message size is limited by the MTU size of
the underlying link).")

(define-symbol-macro default-inet-address-family (if *ipv6* sockint::af-inet6 sockint::af-inet))
(define-symbol-macro default-inet-address-family-keyword (if *ipv6* :ipv6 :ipv4))

;;; Types
(deftype port () "Port number" '(integer 0 65535))
(deftype unprivileged-port () "Unprivileged port number" '(or (integer 1024 65535) (integer 0)))
(deftype privileged-port () "Privileged port number" '(integer 1 1023))
(deftype ip-address () "IP Address specifier" '(or string (vector unsigned-byte) list))
(deftype socket-address () "A complete internet socket address specifier." '(cons ip-address port))

;;; Conditions
(define-condition net-condition () ())
(define-condition net-error (net-condition std-error) ())
(define-condition net-warning (net-condition std-warning) ())

;;; Protocol
(defgeneric address (self))
(defgeneric connection (self))

(defgeneric make-client (kind &rest initargs &key &allow-other-keys))
(defgeneric make-server (kind &rest initargs &key &allow-other-keys))

(defgeneric make-client-request (self req &rest args &key &allow-other-keys))

(defgeneric make-server-response (self res &rest args &key &allow-other-keys))

(defgeneric send-message (message connection)
  (:documentation "Send an encoded message to the server.  The
operation will force (but not finish) output before returning."))

(defgeneric connection-server-address (connection)
  (:documentation "Return the address of the server associated with
the connection."))

(defgeneric connection-server-id (connection)
  (:documentation "Return the unique ID of the server associated with
the connection."))

(defgeneric (setf connection-server-id) (id connection)
  (:documentation "Set the unique ID of the server associated with the
connection.  If an ID is already set and is not EQUAL to the new ID,
signal a continuable error."))

(defgeneric connection-fd (connection)
  (:documentation "Return the file descriptor associated with
the (open) connection."))

(defgeneric connection-pending-messages (connection)
  (:documentation "Return a list of the currently pending messages
associated with the connection, from newest to oldest."))

(defgeneric (setf connection-pending-messages) (new-list connection)
  (:documentation "Set the list of currently pending messages
associated with the connection."))

(defgeneric connection-next-serial (connection)
  (:documentation "Return a 32-bit integer for associating request
messages and their replies."))

(defgeneric drain-pending-messages (connection)
  (:documentation "Return a list of the currently pending messages
associated with the connection, from oldest to newest, and consider
these messages no longer pending."))

(defgeneric wait-for-reply (serial connection)
  (:documentation "Wait for a reply message with the supplied serial
to be received via connection."))

(defgeneric receive-message-no-hang (connection)
  (:documentation "Read a D-BUS message from the server.  If no
message is available to read, return NIL."))

(defverb connect (self &key &allow-other-keys))
(defverb disconnect (self &key &allow-other-keys))

;;; Config
(defconfig net-config (id) 
  ((host)
   (port)))

(defconfig socket-config (net-config)
  ((reuse-address)
   (reuse-port)
   (bind-to-device)
   (type)
   (debug)
   (dont-route)
   (pass-cred)
   (peer-cred)
   (linger)
   (send-buffer)
   (receive-buffer)
   (send-timeout)
   (receive-timeout)
   (priority)))

(defconfig client-config (net-config) ())
(defconfig server-config (net-config) ())

;;; Classes
(defclass connection () ()
  (:documentation "Base class of connection objects between network nodes."))

(defclass route (obj:edgex) ()
  (:documentation "Base class of route objects which may be spawned by a router. Compatible with
the EDGE and ID protocols."))

(defclass wrapped-socket ()
  ((socket :initarg :socket
           :accessor socket)
   (queue :accessor queue)
   (state :accessor state)))

(defmethod socket-close :before ((self wrapped-socket) &key abort)
  (declare (ignore abort))
  (when (slot-boundp self 'queue)
    (remove-element (queue self) self)))

(defmethod socket-open-p ((self wrapped-socket))
  (socket-open-p (socket self)))

(defmethod socket-close ((self wrapped-socket) &key abort)
  (socket-close (socket self) :abort abort))

(defmethod socket-make-stream ((socket wrapped-socket) &rest args &key (output t) (input t) &allow-other-keys)
  (apply 'socket-make-stream (socket socket) :output output :input input args))

(defmethod make-sockaddr-for ((socket wrapped-socket) &optional sockaddr &rest address)
  (apply 'make-sockaddr-for (socket socket) sockaddr address))

(defmethod size-of-sockaddr ((socket wrapped-socket))
  (size-of-sockaddr (socket socket)))

(defmethod free-sockaddr-for ((socket wrapped-socket) sockaddr)
  (when sockaddr (free-sockaddr-for (socket socket) sockaddr)))

(defmethod socket-connect ((socket wrapped-socket) &rest sockaddr)
  (apply 'socket-connect (socket socket) sockaddr))

(defmethod socket-bind ((socket wrapped-socket) &rest sockaddr)
  (apply 'socket-bind (socket socket) sockaddr))

(defclass stream-socket (wrapped-socket wrapped-stream) ()
  (:documentation "A Streaming socket which may be closed with either SOCKET-CLOSE or by closing
the associated stream (accessed with STREAM-OF) with CLOSE."))

(defclass stream-server-socket (wrapped-socket)
  ((element-type
    :initarg :element-type
    :initform 'base-char
    :reader element-type
    :documentation "Default element type of streams created by SOCKET-ACCEPT."))
  (:documentation "A Socket which listens for stream connections to be initiated by remote
sockets."))

(defclass datagram-socket (wrapped-socket)
  ((connected-p :type boolean
                :accessor connected-p
                :initarg :connected-p)))

(defclass client (wrapped-socket obj:node) ()
  (:documentation "Base class of client objects which wrap a socket and may be treated as nodes
in a NETWORK-GRAPH."))

(defmethod make-client-request ((self client) req &key) (nyi!))

(defclass server (wrapped-socket obj:node) ()
  (:documentation "Base class of server objects which wrap a socket and may be treated as nodes
in a NETWORK-GRAPH."))

(defmethod make-server-response ((self server) res &key) (nyi!))

(defclass router (obj:vertex) ()
  (:documentation "Base class of router objects which may spawn routes. Compatible with the NODE
and ID protocols."))

(defclass proxy (server route) ()
  (:documentation "Base class of proxy objects which are servers that can act like a route."))

(defclass endpoint (obj:vertex) ()
  (:documentation "Base class of endpoint objects containing a network address."))

(defclass peer (endpoint) ()
  (:documentation "A network-accessible peer."))

(defclass network-graph (obj:graph) ()
  (:documentation "Graph representation of networks containing clients,
servers, and peers as vertices and connections as edges."))

;;; Protocol
(defmacro with-open-connection ((sym addr &rest args) &body body)
  `(let ((,sym (connect ,addr ,@args)))
     (unwind-protect
          (progn ,@body)
       (when ,sym
         (disconnect ,sym)))))
