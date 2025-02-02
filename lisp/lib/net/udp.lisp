;;; net/udp.lisp --- UDP utilities

;;

;;; Code:
(in-package :net/udp)

(defvar *udp-ping-size* 512)

(defun udp-echo (port)
  (let ((s (make-instance 'inet-socket :type :datagram :protocol :udp)))
    (socket-bind s #(0 0 0 0) port)
    (loop
          (multiple-value-bind (buf len addr port) (socket-receive s nil 500)
          (format t "Received ~A bytes from ~A:~A - ~A ~%"
                  len addr port (subseq buf 0 (min 10 len)))))))
  
(defun udp-receive-ping (port &key (count 16))
  (let ((s (make-instance 'inet-socket :type :datagram :protocol :udp)))
    (socket-bind s #(0 0 0 0) port)
    (loop for i from 0 upto count
          do (multiple-value-bind (buf len address port) (socket-receive s nil *udp-ping-size*)
               (format t "(~A) Received ~A bytes from ~A:~A - ~A ~%"
                       i len address port (subseq buf 0 (min 10 len))))
          finally (socket-close s))))

(defmacro with-udp-client ((socket-var &key (addr #(0 0 0 0)) (port 0) peer) &body body)
  `(let ((,socket-var (make-instance 'inet-socket :type :datagram :protocol :udp)))
     (unwind-protect
          (progn
            (socket-bind ,socket-var ,addr ,port)
            ,(when peer `(apply #'socket-connect ,socket-var ,peer))
            ,@body)
       (socket-close ,socket-var))))

;;; Objects
(defclass udp-socket (inet-socket) ()
  (:default-initargs :type :datagram :protocol :udp))
(defclass udp-client (udp-socket client) ())
(defclass udp-server (udp-socket server) ())
(defclass udp-sink (udp-client sink) ())
(defclass udp-source (udp-server source) ())

;;; MTU Discovery

;; ref: https://www.rfc-editor.org/rfc/rfc9000.html#section-14.3
;; ref: https://github.com/quinn-rs/quinn/blob/main/quinn-proto/src/config/transport.rs (MtuDiscoveryConfig)
