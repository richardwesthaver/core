;;; lib/pod/obj.lisp --- Pod Objects

;;

;;; Code:
(in-package :pod)

(defun start-podman-service (&key local))

(defclass podman-client (client)
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

(defmethod socket-connect ((self podman-client) &rest addr)
  (socket-connect (client-socket self) (or addr (client-peer self))))

(defmethod socket-close ((self podman-client) &key (abort t))
  (socket-close (client-socket self) :abort abort))

(defmethod socket-shutdown ((self podman-client) &key (direction t))
  (socket-shutdown self :direction direction))

(defmethod socket-bind ((self podman-client) &rest addr)
  (socket-bind (client-socket self) (or addr (client-addr self))))

(defmethod socket-make-stream ((self podman-client) 
                               &key (input t) (output t) 
                                 (element-type t) 
                                 (external-format t) 
                                 (buffering t) 
                                 (timeout t))
  (socket-make-stream (client-socket self) 
                      :input input
                      :output output
                      :element-type element-type
                      :external-format external-format
                      :buffering buffering
                      :timeout timeout))
