(in-package :net/proto/crew)

(defclass crew-connection-info ()
  ((host-name :type string
              :documentation "Host this worker is running on.")
   (port :type port
         :documentation "Port on which the worker's swank server is listening for connections.")))

(defclass crew-worker ()
  ((connection-info :type crew-connection-info)
   (lock :initform (make-mutex :name "worker"))
   (connection :type (or null swank-connection)
               :initform nil))
  (:documentation "A remote Lisp running a Swank server."))

(defclass crew-worker-pool (id)
  ((connect-info :type crew-connection-info)
   (workers :type vector :initform (vector))
   (lock :initform (make-mutex :name "worker-pool"))
   (idle-workers :type list :initform nil)
   (worker-available :initform (make-gate))
   (disconnecting :initform nil)
   (replay-forms-lock :initform (make-mutex :name "replay-forms-lock"))
   (replay-forms :type list :initform nil)))

(defvar *crew-worker-pools-lock* (make-mutex :name "worker-pools-lock")
  "Lock protecting access to *WORKER-POOLS*.")

(defvar *crew-worker-pools* (make-hash-table) "Mapping from worker pool IDs to active worker pools.")

(defmethod initialize-instance :after ((self crew-worker-pool) &key)
  (with-mutex (*crew-worker-pools-lock*)
    (setf (gethash (id self) *crew-worker-pools*) self)))

(defgeneric connect-worker (info hook)
  (:documentation
   "Creates a connection a worker's Swank server using INFO. Passes
   thunk HOOK to SWANK-CLIENT:SLIME-CONNECT so that it is
   evoked when the connection closes."))

(defgeneric reconnect-worker (info hook)
  (:documentation
   "Reconnects to a Swank server using information in INFO.  Passes the
thunk HOOK to SWANK-CLIENT:SLIME-CONNECT, so that it is invoked when
the connection closes."))
