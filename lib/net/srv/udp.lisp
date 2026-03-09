;;; udp.lisp --- UDP Services

;; Simple UDP Services

;;; Commentary:

;; UDP services are stateless so typically demand more from the programmer
;; during implementation to be useful.

;; This module intends to glue together the NET/UDP and various NET/CODEC
;; packages (such as TLV) to support rapid development of message-based
;; protocols.

;;; Code:
(in-package :net/srv/udp)

(defconfig udp-service-config (net-service-config) ())

(defmethod make-config ((self (eql :udp)) &rest args &key)
  (apply 'make-instance 'udp-service-config args))

(defclass udp-service-request (net-request) ())

(defclass udp-service-response (net-response) ())

(defun get-udp-request-data (stream &optional spec)
  "Read an incoming UDP packet. An optional SPEC may be provided which should
contain a list of packet specifications used to advise the algorithm on how to
read packets. The spec list should be ordered by descending priority with the
highest priority packet spec first.

On success two values are returned: (PROTO HEADERS)."
  ;; TODO 2025-07-24: 
  (declare (ignore stream spec)))

;;; Service
(defclass udp-service (net-service udp-server) ()
  (:default-initargs
   :request-class 'udp-service-request
   :response-class 'udp-service-response
   :type :datagram
   :protocol :udp
   :engine (make-instance 'thread-per-connection-engine :name :udp)))

(defmethod accept ((self udp-service)) 
  ())

(defmethod process-connection ((*service* udp-service) (socket t))
  "UDP does not maintain connections between peers, but the default engine
type (THREAD-PER-CONNECTION-ENGINE) expects the connection protocol to be
  implemented. We implement this function for UDP-SERVICE to allow a simple
  handshake to be performed which registers a connection from the peer on
  SOCKET."
  (let* ((socket-stream (socket-make-stream socket))
         (*service-stream*)
         (*close-service-stream* t)
         (remote (multiple-value-list (socket-peername socket)))
         (local (multiple-value-list (socket-name socket))))
    (progn
      (setq *service-stream* (initialize-connection-hook *service* socket-stream))
      (loop
            (let ((*finish-processing-socket* t))
              (when (shutdown-p *service*)
                (return))
              (multiple-value-bind (proto headers)
                  (get-udp-request-data *service-stream*)
                (let ((*response* (make-instance (service-response-class *service*)))
                      (*session* nil))
                  (with-request-count-incf *service*
                    (process-request 
                     (service-make-request
                      *service* socket 
                      :headers-in headers
                      :protocol proto 
                      :remote remote
                      :local local
                      :content-stream *service-stream*))))
                (finish-output *service-stream*)
                (setq *service-stream* (reset-connection-stream *service* *service-stream*))
                (when *finish-processing-socket*
                  (return))))))
    (when *close-service-stream*
      (flet ((close-stream (stream)
               ;; as we are at the end of the request here, we ignore all
               ;; errors that may occur while flushing and/or closing the
               ;; stream.
               (ignore-errors
                (finish-output stream))
               (ignore-errors
                (close stream :abort t))))
        (unless (or (not *service-stream*)
                    (eql socket-stream *service-stream*))
          (close-stream *service-stream*))
        (close-stream socket-stream)))))

(defmethod process-request ((req udp-service-request)))
