;;; lib/pod/client.lisp --- Libpod API client

;;

;;; Code:
(in-package :pod)

(declaim (inline podman-local-user-socket))
(defun podman-local-user-socket () 
  (format nil "/var/run/user/~a/podman/podman.sock" (sb-posix:getuid)))

(defvar *libpod-api-version* "4.8.2")

;;; TODO Socket
(defclass libpod-unix-socket (local-socket) ())

(defclass libpod-tcp-socket (inet-socket) ())

;;; Client
(defclass libpod-client (net/req:http-client)
  ((socket :initarg :socket 
           :initform (make-instance 'local-socket :type :stream)
           :type (or local-socket null)
           :accessor client-socket)
   (addr :initarg :addr
         :initform nil
         :accessor client-addr)
   (peer :initarg :peer
         :initform (podman-local-user-socket)
         :accessor client-peer))
  (:default-initargs 
   :kernel #'libpod-request
   :ser #'json:json-encode
   :de #'json:json-decode))

(defmethod make-load-form ((self libpod-client) &optional env)
  (declare (ignore env))
  `(make-instance 'libpod-client :socket nil :addr ,(client-addr self) :peer ,(client-peer self)))

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

(defmacro with-libpod-client ((cvar &optional (c (make-instance 'libpod-client))) &body body)
  `(let ((,cvar ,c))
     (progn
       (socket-connect ,cvar)
       (unwind-protect (progn ,@body)
         (socket-close ,cvar)))))

(defun format-libpod-api-local (path)
  (format nil "http://localhost/v~a/libpod/~a" *libpod-api-version* path))

(defun libpod-request (client path &optional (method :get) timeout)
  (let ((stream (socket-make-stream client
                                    :element-type 'octet
                                    :input t
                                    :output t
                                    :buffering :none)))
    (let ((wrapped-stream (make-chunked-stream stream)))
      (funcall (kernel client) 
               (format-libpod-api-local path)
               :method method
               :stream wrapped-stream
               :connect-timeout #1=(or timeout t)
               :read-timeout #1#))))

(defun libpod-request-json (client path &optional (method :get) timeout)
  (dat/json:json-decode (libpod-request client path method timeout)))

(defmethod send-request ((self libpod-client) req &rest args)
  (apply (kernel self) self req args))
