;;; net/tcp.lisp --- Network Objects

;;

;;; Code:
(in-package :net/core)

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

;;; Generics
(defgeneric socket-element-type (self)
  (:documentation "Return the element-type of the underlying stream or buffer of socket SELF."))

;;; Classes
(defconfig net-config (id) 
  ((addr)
   (port)))

(defclass connection (obj:edgex) ()
  (:documentation "Base class of connection objects between network nodes. Compatible with the
EDGE and ID protocols."))

(defclass route (obj:edgex) ()
  (:documentation "Base class of route objects which may be spawned by a router. Compatible with
the EDGE and ID protocols."))

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

(defclass wrapped-socket ()
  ((socket :initarg :socket
           :accessor socket)
   (queue :accessor queue)
   (state :accessor state)))

(defmethod socket-close :before ((self wrapped-socket) &key abort)
  (declare (ignore abort))
  (when (slot-boundp self 'queue)
    (remove-element (queue self) self)))

(defclass stream-socket (wrapped-socket wrapped-stream) ()
  (:documentation "A Streaming socket which may be closed with either SOCKET-CLOSE or by closing
the associated stream (accessed with STREAM-OF) with CLOSE."))

(defclass stream-server-socket (wrapped-socket)
  ((element-type
    :initarg :element-type
    :initform 'base-char
    :reader socket-element-type
    :documentation "Default element type of streams created by SOCKET-ACCEPT."))
  (:documentation "A Socket which listens for stream connections to be initiated by remote
sockets."))

(defclass datagram-socket (wrapped-socket)
  ((connected-p :type boolean
                :accessor connected-p
                :initarg :connected-p)))

(defclass client (wrapped-socket obj:vertex) ()
  (:documentation "Base class of client objects which wrap a socket and may be treated as nodes
in a NETWORK-GRAPH."))

(defconfig client-config (net-config) ())

(defclass server (wrapped-socket obj:vertex) ()
  (:documentation "Base class of server objects which wrap a socket and may be treated as nodes
in a NETWORK-GRAPH."))

(defconfig server-config (net-config) ())

(defclass router (obj:vertex) ()
  (:documentation "Base class of router objects which may spawn routes. Compatible with the NODE
and ID protocols."))

(defclass proxy (server route) ()
  (:documentation "Base class of proxy objects which are servers that can act like a route."))

(defclass peer (obj:vertex) ()
  (:documentation "Base class of peer objects which represent network peers in the NETWORK-GRAPH."))

(defclass network-graph (obj:graph) ()
  (:documentation "Graph representation of networks containing clients,
servers, and peers as vertices and connections as edges."))

;;; Protocol
(defverb connect (self &key &allow-other-keys))
(defverb disconnect (self &key &allow-other-keys))

(defgeneric make-client (kind &rest initargs &key &allow-other-keys))
(defgeneric make-server (kind &rest initargs &key &allow-other-keys))

(defgeneric make-client-request (self req &rest args &key &allow-other-keys)
  (:method ((self client) req &key) (nyi!)))

(defgeneric make-server-response (self res &rest args &key &allow-other-keys)
  (:method ((self server) res &key) (nyi!)))
