;;; socket.lisp --- High-level Socket API

;; based on IOLib's make-socket.lisp

;;; Code:
(in-package :net/core)

;; client-socket = active-socket
;; server-socket = passive-socket
(defun make-socket (&rest args &key (family :internet) (type :stream) (connection-type :client) connect (ipv6 *ipv6*) (protocol *default-inet-protocol*) port &allow-other-keys)
  (check-type family (member :internet :inet :unix :local :ipv4 :ipv6 :netlink)
              "one of :INTERNET(or :INET), :LOCAL(or :FILE, :UNIX), :IPV4, :IPV6 or :NETLINK")
  (check-type type (member :stream :datagram :raw) "either :STREAM, :DATAGRAM or :RAW")
  (check-type connection-type (or null (member :client :server)) "either :CLIENT, :SOCKET or NIL")
  (when (eql :ipv4 family) (setf ipv6 nil))
  (let ((*ipv6* ipv6)
        (args (remove-from-plist args :port)))
    (when (or (eql :internet family)
              (eql :inet family))
      (setq family default-inet-address-family-keyword))
    (let ((sock
            (case connection-type
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
      (if connect 
          (values (apply 'socket-connect sock (if (atom connect) 
                                                  (list connect port)
                                                  connect))
                  (socket-make-stream sock))
          sock))))

(defmacro with-open-socket ((sock &rest args) &body body)
  `(let ((,sock (make-socket ,@args)))
     ,@body))

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
