;;; net/sans-io.lisp --- No-I/O protocol abstraction

;; https://sans-io.readthedocs.io/

;;; Commentary:

;;

;;; Code:
(in-package :net/sans-io)
;;; Abstract
(defclass sans-io-protocol ()
  ((version :initarg :version)
   (features :initarg :features)))
(defmethod protocol-version ((self sans-io-protocol)) 0)
(defmethod protocol-name ((self sans-io-protocol)) "sans-io")
;;; Parameters
(defvar *word-length* 64)
(defvar *max-connection-id* sb-ext:most-positive-word)
(defvar *initial-mtu* 1200)
(defvar *max-udp-payload* 65527)
(defvar *max-stream-count* (ash 1 60))
;;; Errors
(define-condition sans-io-error (std-error) ())
(define-condition packet-serializer-error (serializer-error) ())
(define-condition packet-deserializer-error (deserializer-error) ())
(define-condition packet-header-serializer-error (serializer-error) ())
(define-condition packet-header-deserializer-error (deserializer-error) ())
(define-condition frame-serializer-error (serializer-error) ())
(define-condition frame-deserializer-error (deserializer-error) ())
;;; IO
(defclass stream-id (id) ())
(defclass byte-buffer () ())
(defclass datagram-buffer () ())
(defgeneric stream-direction ())
;;; Events
(defclass event-id (id) ())
(defmethod make-id (&optional (self (eql :event)))
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
(defmethod make-id (&optional (self (eql :connection))) 
  (declare (ignorable self))
  (make-instance 'connection-id))
(defmethod reset-id ((self connection-id)) (setf (id self) 0))
(defmethod update-id ((self connection-id)) (setf (id self) (hash-object self)))
(defclass connection (connection-id) ())
(defclass connection-idle-timeout ()
  ((timeout :initform 10000 ;; 10 seconds
            :type (integer 0 most-positive-fixnum))))
;;; Peers
(defclass peer-id (id) ())
(defmethod make-id (&optional (self (eql :peer)))
  (declare (ignorable self))
  (make-instance 'peer-id))
(defmethod reset-id ((self peer-id)) (setf (id self) 0))
(defmethod update-id ((self peer-id)) (setf (id self) (hash-object self)))
(defclass peer-address (id) ())
(defclass peer (peer-id peer-address) ())
(defgeneric clientp ())
(defgeneric serverp ())
;;; Endpoints
(defclass endpoint-config ()
  ((socket :initarg :socket :type socket)
   (id-factory :initarg :id-factory :type id-factory)
   (features :initarg :features)))

(defclass transport-config () ())

(defclass server-config ()
  ((transport :initarg :transport :type transport-config)))

(defclass client-config ()
  ((transport :initarg :transport :type transport-config)))

(defclass endpoint (endpoint-config connection-id-generator)
  ((connections :initform #() :type (array connection))
   (server :initarg :server :initform nil :type (or null server-config))))
(defgeneric handle-event ())
(defgeneric handle ())
(defgeneric connect ())
(defgeneric default-client-config ())
;;; Packets
(defclass packet-number (id) ())
(defclass packet-header (packet-number) ())
(defclass packet-payload () (payload))
(defclass packet (packet-header packet-payload) ())
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
(defmacro define-protocol (&environment env name from-protocols &rest options))
(defmacro define-endpoint (name &rest options))
(defmacro define-event (name &rest options))
(defmacro define-handler (name &body body))
(defmacro with-endpoint ())
(defmacro with-client ())
