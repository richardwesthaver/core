;;; socket.lisp --- High-level Socket API

;; based on IOLib's make-socket.lisp

;;; Code:
(in-package :net/core)

;; client-socket = active-socket
;; server-socket = passive-socket
(defun make-socket (&rest args &key (family :internet) (type :stream) (class :client) 
                                    (ipv6 *ipv6*) (protocol *default-inet-protocol*) 
                                    (local-host *wildcard-host*) (local-port *wildcard-port*) 
                                    remote-host remote-port &allow-other-keys)
  (check-type family (member :internet :inet :unix :local :ipv4 :ipv6 :netlink)
              "one of :INTERNET(or :INET), :LOCAL(or :FILE, :UNIX), :IPV4, :IPV6 or :NETLINK")
  (check-type type (member :stream :datagram :raw) "either :STREAM, :DATAGRAM or :RAW")
  (check-type class (or null (member :client :server)) "either :CLIENT, :SOCKET or NIL")
  (when (eql :ipv4 family) (setf ipv6 nil))
  (let ((*ipv6* ipv6)
        (args (remove-from-plist args :remote-host :local-host :local-port :remote-port :bind :class :connect :ipv6)))
    (when (or (eql :internet family)
              (eql :inet family))
      (setq family default-inet-address-family-keyword))
    (let ((sock
            (case class
              (:client
               (case family
                 (:ipv4 (make-instance 'client :socket (apply 'make-instance 'inet-socket :type type :protocol protocol args)))
                 (:ipv6 (make-instance 'client :socket (apply 'make-instance 'inet6-socket :type type :protocol protocol args)))
                 (:local (make-instance 'client :socket (apply 'make-instance 'local-socket :type type :protocol protocol args)))))
              (:server
               (case family
                 (:ipv4 (make-instance 'server :socket (apply 'make-instance 'inet-socket :type type :protocol protocol args)))
                 (:ipv6 (make-instance 'server :socket (apply 'make-instance 'inet6-socket :type type :protocol protocol args)))
                 (:local (make-instance 'server :socket (apply 'make-instance 'local-socket :type type :protocol protocol args)))))
              (t 
               (case family
                 (:ipv4 (apply 'make-instance 'inet-socket args))
                 (:ipv6 (apply 'make-instance 'inet6-socket args))
                 (:local (apply 'make-instance 'local-socket args))
                 (:netlink (apply 'make-instance 'netlink-socket args)))))))
      (when local-host 
        (apply 'socket-bind sock (etypecase local-host 
                                   (string (list (get-address-by-name local-host) local-port))
                                   (vector (list local-host local-port))
                                   (list local-host))))
      (when remote-host
        (apply 'socket-connect sock (etypecase remote-host
                                      (string (list (get-address-by-name remote-host) remote-port))
                                      (vector (list remote-host remote-port))
                                      (list remote-host))))
      (if (or local-host remote-host)
          (values sock (socket-make-stream sock))
          sock))))

(defmacro with-open-socket ((sock &rest args &key (close *socket-auto-close*) abort &allow-other-keys) &body body)
  (let ((svar (if (atom sock) sock (car sock))))
    `(multiple-value-bind (,@(if (atom sock) `(,sock) sock)) (make-socket ,@args)
       ,@(if (or close abort)
             `((unwind-protect (progn ,@body) (when (socket-open-p ,svar) (socket-close ,svar :abort ,abort))))
             body))))

#+todo
(defun ping (target &key (id #xFF) (seqno 1))
  (with-open-socket (socket :family :ipv4 :type :raw :protocol sockint::ipproto_icmp
                            :include-headers t)
    (let* ((payload-size 4)
           (icmp-packet-size (+ (alien-size icmp-header) payload-size))
           (frame-size (+ (alien-size ip-header) icmp-packet-size)))
      (std:with-foreign-object (frame 'unsigned-char frame-size)
        (std:memset frame 0 frame-size)
        (let* ((ip-header frame)
               (icmp-header (sb-sys:sap+ ip-header (alien-size ip-header)))
               (payload (sb-sys:sap+ icmp-header (alien-size icmp-header))))
          (write-ip-header ip-header frame-size (dotted-to-integer target))
          (setf (std:sap-ref payload unsigned-int) (htonl #x1A2B3C4D))
          (write-icmp-header icmp-header icmp-packet-size id seqno)
          (send-to socket frame :end frame-size :remote-host target)
          (wait-until-fd-ready (sb-bsd-sockets::socket-file-descriptor socket) :input)
          (receive-from socket :size (* 64 1024)))))))
