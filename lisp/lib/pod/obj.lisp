;;; lib/pod/obj.lisp --- Pod Objects

;;

;;; Code:
(in-package :pod)

;;; Client
(defclass libpod-client (client)
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

;;; Net Client protocol

;;; Socket Protocol
(defmethod socket-connect ((self libpod-client) &rest addr)
  (socket-connect (client-socket self) (or addr (client-peer self))))

(defmethod socket-close ((self libpod-client) &key (abort t))
  (socket-close (client-socket self) :abort abort))

(defmethod socket-shutdown ((self libpod-client) &key (direction t))
  (socket-shutdown self :direction direction))

(defmethod socket-send ((self libpod-client) buffer length 
                        &key address
                          external-format
                          oob
                          eor
                          dontroute
                          dontwait
                          nosignal
                          confirm
                          more)
  (socket-send (client-socket self) buffer length 
               :address address
               :external-format external-format
               :oob oob
               :eor eor
               :dontroute dontroute
               :dontwait dontwait
               :nosignal nosignal
               :confirm confirm
               :more more))

(defmethod socket-receive ((self libpod-client) buffer length 
                        &key (oob t)
                          (peek t)
                          (waitall t)
                          (dontwait t)
                          (element-type 'character))
  (socket-receive (client-socket self) buffer length 
               :element-type element-type
               :oob oob
               :dontwait dontwait
               :waitall waitall
               :peek peek))

(defmethod socket-listen ((self libpod-client) backlog)
  (socket-listen (client-socket self) backlog))

(defmethod socket-bind ((self libpod-client) &rest addr)
  (socket-bind (client-socket self) (or addr (client-addr self))))

(defmethod socket-accept ((self libpod-client))
  (socket-accept (client-socket self)))

(defmethod socket-make-stream ((self libpod-client) 
                               &key input output
                                 (element-type 'character) 
                                 (external-format :default)
                                 (buffering :full)
                                 timeout
                                 auto-close
                                 serve-events)
  (socket-make-stream (client-socket self)
                      :input input
                      :output output
                      :element-type element-type
                      :external-format external-format
                      :buffering buffering
                      :timeout timeout
                      :auto-close auto-close
                      :serve-events serve-events))
