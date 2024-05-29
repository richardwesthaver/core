;;; net/sans-io.lisp --- No-I/O protocol abstraction

;; https://sans-io.readthedocs.io/

;;; Commentary:

;;

;;; Code:
(in-package :net/sans-io)

;;; Abstract
(defclass sans-io-protocol ()
  ((version :initarg :version :accessor protocol-version)
   (features :initarg :features :accessor protocol-features)))

(defmethod protocol-name ((self sans-io-protocol)) (class-name (class-of self)))

;;; Parameters
(defvar *word-length* 64)
(defvar *max-connection-id* sb-ext:most-positive-word)
(defvar *initial-mtu* 1200)
(defvar *max-udp-payload* 65527)
(defvar *max-stream-count* (ash 1 60))

;;; Errors
(define-condition sans-io-error (protocol-error) ())
(define-condition packet-serializer-error (sans-io-error serializer-error) ())
(define-condition packet-deserializer-error (sans-io-error deserializer-error) ())
(define-condition packet-header-serializer-error (sans-io-error serializer-error) ())
(define-condition packet-header-deserializer-error (sans-io-error deserializer-error) ())
(define-condition frame-serializer-error (sans-io-error serializer-error) ())
(define-condition frame-deserializer-error (sans-io-error deserializer-error) ())

;;; IO
(defclass stream-id (id) ())
(defclass byte-buffer () ())
(defclass datagram-buffer () ())

(defgeneric stream-direction ())

;;; Events
(defclass event-id (id) ())

(defmethod make-id ((self (eql :event)))
  (declare (ignorable self))
  (make-instance 'event-id))
(defmethod reset-id ((self event-id)) (setf (id self) 0))
(defmethod update-id ((self event-id)) (setf (id self) (hash-object self)))

(defclass event (event-id) ())

(defclass endpoint-event (event) ())
(defclass connection-event (event) ())

;;; Connections
(defclass connection-id (id) ())

(defclass connection-id-generator () ())
(defmethod make-id ((self (eql :connection)))
  (declare (ignorable self))
  (make-instance 'connection-id))
(defmethod reset-id ((self connection-id)) (setf (id self) 0))
(defmethod update-id ((self connection-id)) (setf (id self) (hash-object self)))

(defclass connection (connection-id) ())

(defclass connection-idle-timeout ()
  ((timeout :initform 10000 ;; 10 seconds
            :type (integer 0 *))))

;;; Peers
(defclass peer-id (id) ())

(defmethod make-id ((self (eql :peer)))
  (declare (ignorable self))
  (make-instance 'peer-id))
(defmethod reset-id ((self peer-id)) (setf (id self) 0))
(defmethod update-id ((self peer-id)) (setf (id self) (hash-object self)))

(defclass peer-address () ((address :initarg :address)))

(defclass peer (peer-id peer-address) ())

(defgeneric clientp (self)
  (:documentation "Return non-nil if SELF is a valid CLIENT."))

(defgeneric serverp ()
  (:documentation "Return non-nil if SELF is a valid SERVER."))

;;; Endpoints
(defclass endpoint-config ()
  ((socket :initarg :socket :type socket)
   (id-factory :initarg :id-factory :type id-factory)
   (features :initarg :features))
  (:documentation "Configuration for ENDPOINTs, affecting all connections."))

(defclass transport-config () ()
  (:documentation "Configuration for a network protocol state machine."))

(defclass server-config (transport-config) ())

(defclass client-config (transport-config) ())

(defclass endpoint (endpoint-config connection-id-generator)
  ((connections :initform #() :type (array connection))
   (server :initarg :server)
   (client :initarg :client)))

(defgeneric handle-event ())
(defgeneric handle ())
(defgeneric connect ())
(defgeneric default-client-config ())

;;; Packets
(defclass packet-number (id) ())
(defclass packet-header (packet-number) (header))
(defclass packet-payload () (payload))
(defclass packet (packet-payload) ())

(defmethod serialize ((self packet) format &key &allow-other-keys))
(defmethod deserialize ((self packet) format &key &allow-other-keys))

(defmethod serialize ((self packet-header) format &key &allow-other-keys))
(defmethod deserialize ((self packet-header) format &key &allow-other-keys))

;;; Frames
(defclass frame () ())

(defgeneric size-bound ())
(defgeneric frame-type ())

(defmethod serialize ((self frame) format &key &allow-other-keys))
(defmethod deserialize ((self frame) format &key &allow-other-keys))

;;; Macros
(defmacro define-protocol (name superclasses slots &key version features)
  "Define a network protocol based on SANS-IO-PROTOCOL."
  `(defclass ,name (,@(or superclasses (list 'sans-io-protocol)))
     ,slots
     (:default-initargs
      :version ,version
      :features ,features)))

;; (defmacro define-endpoint (name &rest options))
;; (defmacro define-event (name &rest options))
;; (defmacro define-handler (name &body body))
(defmacro with-endpoint ())
(defmacro with-client ())
