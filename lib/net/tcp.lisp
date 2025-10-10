;;; net/tcp.lisp --- TCP utilities

;;

;;; Code:
(in-package :net/tcp)

;;; Utils
(defun tcp-echo (port)
  (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (socket-bind s #(0 0 0 0) port)
    (loop
      (multiple-value-bind (buf len addr port) (socket-receive s nil 500)
        (format t "Received ~A bytes from ~A:~A - ~A ~%"
                len addr port (subseq buf 0 (min 10 len)))))))

(defvar *tcp-ping-size* 512)

(defun tcp-receive-ping (port &key (count 16))
  (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (socket-bind s #(0 0 0 0) port)
    (loop for i from 0 upto count
          do (multiple-value-bind (buf len address port) (socket-receive s nil *tcp-ping-size*)
               (format t "(~A) Received ~A bytes from ~A:~A - ~A ~%"
                       i len address port (subseq buf 0 (min 10 len))))
          finally (socket-close s))))

(defmacro with-tcp-client ((socket-var &key (addr #(0 0 0 0)) (port 0) peer) &body body)
  `(let ((,socket-var (make-instance 'inet-socket :type :stream :protocol :tcp)))
     (unwind-protect
          (progn
            (socket-bind ,socket-var ,addr ,port)
            ,(when peer `(apply #'socket-connect ,socket-var ,peer))
            ,@body)
       (socket-close ,socket-var))))

;;; Objects
(defconfig tcp-config (net-config) ())

(defclass tcp-socket (socket) 
  ((sb-bsd-sockets::family :initform sockint::af-inet))
  (:default-initargs :type :stream :protocol :tcp))

(defmethod sb-bsd-sockets::make-sockaddr-for ((socket tcp-socket) &optional sockaddr &rest address)
  (apply 'net/core::%sockaddr sockaddr address))

(defclass tcp-client (tcp-socket client) ())
(defclass tcp-server (tcp-socket server) ())
(defclass tcp-source (tcp-server source) ())
(defclass tcp-sink (tcp-client sink) ())
