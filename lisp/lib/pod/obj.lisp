;;; lib/pod/obj.lisp --- Pod Objects

;;

;;; Code:
(in-package :pod)

(defun start-podman-service (&key local))

(defclass podman-client ()
  ((socket :initarg :socket 
           :initform (make-instance 'local-socket :type :stream) 
           :type socket
           :accessor client-socket)
   (addr :initarg :addr
         :initform nil
         :accessor client-addr)
   (peer :initarg :peer
         :initform *podman-local-user-socket*
         :accessor client-peer)))

(defmethod podman-connect ((self podman-client))
  (socket-bind (client-socket self) (client-addr self))
  (socket-connect (client-socket self) (client-peer self))
  self)

(defmethod podman-disconnect ((self podman-client) &optional (abort t))
  (socket-close (client-socket self) :abort abort))
