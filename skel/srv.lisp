;;; srv.lisp --- Skel Service

;; 

;;; Code:
(in-package :skel/srv)

(defclass skel-message (request response) 
  ((type :initarg :type :initform :ack)
   )
  (:documentation "In-memory representation of a binary-encoded, unencrypted, uncompressed
message sent over UDP.

This object should have its DATA slot initialized with an octet-vector which
the remaining slots will be serialized to/from. Messages never store header
information. For a lower-level interface which preserves the header see SK-PACKET."))

(defclass skel-service (service) ()
  (:documentation "Base class for SKEL services.")
  (:default-initargs
   :request-class 'sk-message
   :response-class 'sk-message))

(defclass skel-engine (multi-threaded-engine thread-pool) ())

(defmethod print-object ((self skel-service) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (id:id self))))
