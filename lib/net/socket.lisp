;;; socket.lisp --- High-level Socket API

;; based on IOLib's make-socket.lisp

;;; Code:
(in-package :net/core)

;; client-socket = active-socket
;; server-socket = passive-socket
(defun make-socket (&rest args &key (family :internet) (type :stream) (class :client) 
                                    (ipv6 *ipv6*) (protocol *default-inet-protocol*) 
                                    (host *wildcard-host*) (port *wildcard-port*) 
                                    remote-host remote-port &allow-other-keys)
  (check-type family (member :internet :inet :unix :local :ipv4 :ipv6 :netlink)
              "one of :INTERNET(or :INET), :LOCAL (or :UNIX), :IPV4, :IPV6 or :NETLINK")
  (check-type type (member :stream :datagram :raw) "either :STREAM, :DATAGRAM or :RAW")
  (check-type class (or null (member :client :server)) "either :CLIENT, :SOCKET or NIL")
  (when (eql :ipv4 family) (setf ipv6 nil))
  (let ((*ipv6* ipv6)
        (args (remove-from-plist args :remote-host :host :port :remote-port :bind :class :connect :ipv6)))
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
      (when host 
        (apply 'socket-bind sock (etypecase host 
                                   (string (list (get-address host) port))
                                   (vector (list host port))
                                   (list host))))
      (when remote-host
        (apply 'socket-connect sock (etypecase remote-host
                                      (string (list (get-address remote-host) remote-port))
                                      (vector (list remote-host remote-port))
                                      (list remote-host))))
      (if (or host remote-host)
          (values sock (socket-make-stream sock))
          sock))))

(defmacro with-open-socket ((sock &rest args &key (close *socket-auto-close*) abort &allow-other-keys) &body body)
  (let ((svar (if (atom sock) sock (car sock))))
    `(multiple-value-bind (,@(if (atom sock) `(,sock) sock)) (make-socket ,@args)
       ,@(if (or close abort)
             `((unwind-protect (progn ,@body) (when (socket-open-p ,svar) (socket-close ,svar :abort ,abort))))
             body))))

;;; Socket Utils
(definline %socket-operation-in-progress-p (condition)
  (typep condition 'operation-in-progress)) ;; errno 115 

(definline %socket-not-connected-p (condition)
  (typep condition 'not-connected-error)) ;; errno 107

;; returns an alien struct pointer, allocated based on input
(defun %sockaddr (&optional sockaddr &rest addr)
  (check-type addr (or null (cons sequence (cons (unsigned-byte 16)))))
  (let ((host (first addr))
        (port (second addr)))
    (when (and host port)
      (ecase (length host)
        (16 (let ((sockaddr (or sockaddr (sockint::allocate-sockaddr-in6))))
              (setf (sockint::sockaddr-in6-family sockaddr)
                    sockint::af-inet6
                    (sb-alien:deref (sockint::sockaddr-in6-port sockaddr) 0)
                    (ldb (byte 8 8) port)
                    (sb-alien:deref (sockint::sockaddr-in6-port sockaddr) 1)
                    (ldb (byte 8 0) port))
              (dotimes (i 4)
                (setf (sb-alien:deref (sockint::sockaddr-in6-flowinfo sockaddr) i) 0))
              (dotimes (i 16)
                (setf (sb-alien:deref (sockint::sockaddr-in6-addr sockaddr) i) (elt host i)))
              (dotimes (i 4)
                (setf (sb-alien:deref (sockint::sockaddr-in6-scope-id sockaddr) i) 0))
              sockaddr))
        (4 (let ((sockaddr (or sockaddr (sockint::allocate-sockaddr-in))))
             (let ((in-port (sockint::sockaddr-in-port sockaddr))
                   (in-addr (sockint::sockaddr-in-addr sockaddr)))
               (declare (fixnum port))
               ;; port and host are represented in C as "network-endian" unsigned
               ;; integers of various lengths.  This is stupid.  The value of the
               ;; integer doesn't matter (and will change depending on your
               ;; machine's endianness); what the bind(2) call is interested in
               ;; is the pattern of bytes within that integer.

               ;; We have no truck with such dreadful type punning.  Octets to
               ;; octets, dust to dust.
               (setf (sockint::sockaddr-in-family sockaddr) sockint::af-inet)
               (setf (sb-alien:deref in-port 0) (ldb (byte 8 8) port))
               (setf (sb-alien:deref in-port 1) (ldb (byte 8 0) port))
               (setf (sb-alien:deref in-addr 0) (elt host 0))
               (setf (sb-alien:deref in-addr 1) (elt host 1))
               (setf (sb-alien:deref in-addr 2) (elt host 2))
               (setf (sb-alien:deref in-addr 3) (elt host 3)))
             sockaddr))))))

;; from usocket
(defun get-address-by-name (name)
  "Return the address of a host by NAME."
  (multiple-value-bind (host4 host6)
      (get-host-by-name name)
    (let ((addr4 (when host4
                   (car (sb-bsd-sockets::host-ent-addresses host4))))
          (addr6 (when host6
                   (car (sb-bsd-sockets::host-ent-addresses host6)))))
      (values addr4 addr6))))

(defun get-address (name &optional (ipv6 *ipv6*))
  (multiple-value-bind (v4 v6) (get-address-by-name name)
    (if ipv6 v6 v4)))

;; from https://github.com/eudoxia0/find-port
(defun port-open-p (port &key (host *localhost*))
  "Determine if a PORT is open on the given HOST."
  (handler-case
      (let ((socket (make-instance 'inet-socket :type :stream)))
        (setf (sockopt-reuse-address socket) t)
        (socket-bind socket host port)
        (socket-close socket))
    (address-in-use-error (condition)
      (declare (ignore condition))
      nil)))

(defun find-port (&key (min 2000) (max 65535) (host *localhost*))
  "Return the first available port in a range of port numbers."
  (loop :for port :from min :to max :when (port-open-p port :host host) :return port))

;; (find-port)
;; (get-address-by-name "localhost")

;; (defun make-address (name)
;; (defun ensure-address (address &key (family :inet) abstract (errorp nil)))

;;; Macros
;; TODO 2026-03-10: 
(defmacro with-socket ((var socket) &body body)
  "Bind SOCKET to VAR and eval BODY followed by calling SOCKET-CLOSE on SOCKET."
  (once-only (socket)
    `(let ((,var ,socket))
       (unwind-protect (when ,var . ,body)
         (when ,var (socket-close ,var))))))

(defmacro with-client-socket ((socket-var stream-var &rest args) &body body)
  "Bind the socket resulting from (APPLY 'SOCKET-CONNECT ARGS) to SOCKET-VAR and
if STREAM-VAR is non-nil, also bind the associated socket stream to it."
  `(with-socket (,socket-var (socket-connect . ,args))
     ,(if (null stream-var)
          `(progn . ,body)
          `(let ((,stream-var (stream-of ,socket-var)))
             . ,body))))

(defmacro with-server-socket ((var socket) &body body)
  "Bind SOCKET to VAR, ensuring socket destruction on exit. BODY is only
evaluated when VAR is non-nil."
  `(with-socket (,var ,socket)
     . ,body))

(defmacro with-socket-listener ((var &rest args) &body body)
  "Bind the socket resulting from (APPLY 'SOCKET-LISTEN ARGS) to VAR and eval
BODY."
  `(with-server-socket (,var (socket-listen . ,args)) . ,body))

(defmacro with-socket-connection ((var &rest args) &body body)
  "Bind the result of (APPLY 'SOCKET-ACCEPT ARGS) to VAR and eval BODY."
  `(with-server-socket (,var (socket-accept . ,args)) . ,body))

(defmacro with-client-server (((socket-class &rest common-initargs)
                                   (listen-socket-var &rest listen-address)
                                   (client-socket-var &rest client-address)
                                   server-socket-var)
                                      &body body)
  `(let ((,listen-socket-var (make-instance ',socket-class ,@common-initargs))
         (,client-socket-var (make-instance ',socket-class ,@common-initargs))
         (,server-socket-var))
     (unwind-protect
          (progn
            (setf (sockopt-reuse-address ,listen-socket-var) t)
            (socket-bind ,listen-socket-var ,@listen-address)
            (socket-listen ,listen-socket-var 5)
            (socket-connect ,client-socket-var ,@client-address)
            (setf ,server-socket-var (socket-accept ,listen-socket-var))
            ,@body)
       (socket-close ,client-socket-var)
       (socket-close ,listen-socket-var)
       (when ,server-socket-var
         (socket-close ,server-socket-var)))))

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

;;; TCP
(defconfig tcp-config (socket-config) 
  ((nodelay :type boolean)
   (keepalive :type boolean)
   (keepcnt)
   (keepidle)
   (keepintvl)
   (user-timeout)))

(defclass tcp-socket (socket) ()
  (:default-initargs :type :stream :protocol :tcp :family default-inet-address-family))

(defmethod make-sockaddr-for ((socket tcp-socket) &optional sockaddr &rest address)
  (apply 'net/core::%sockaddr sockaddr address))

(defmethod size-of-sockaddr ((socket tcp-socket))
  (case (socket-family socket)
    (#.sockint::af-inet sockint::size-of-sockaddr-in)
    (#.sockint::af-inet6 sockint::size-of-sockaddr-in6)
    (t (error "unknown sockaddr size"))))

(defmethod free-sockaddr-for ((socket tcp-socket) sockaddr)
  (when sockaddr
    (sb-alien:free-alien sockaddr)))

;;;; Utils
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

;;; UDP
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
(defconfig udp-config (socket-config) 
  ;; checksum (udplite), anycast/multicast
  ((broadcast)))

(defclass udp-socket (socket) 
  ((family :initform default-inet-address-family))
  (:default-initargs :type :datagram :protocol :udp))

(defmethod make-sockaddr-for ((socket udp-socket) &optional sockaddr &rest address)
  (apply 'net/core::%sockaddr sockaddr address))

;; MTU Discovery handled in CLI/TOOLS/NET via /sys/class/net/*/mtu
;; ref: https://www.rfc-editor.org/rfc/rfc9000.html#section-14.3
;; ref: https://github.com/quinn-rs/quinn/blob/main/quinn-proto/src/config/transport.rs (MtuDiscoveryConfig)

;;; Multicast
;; range 224.0.0.0 to 239.255.255.255
;; don't use .1,2,22

;;; UNIX
(defconfig unix-socket-config (socket-config) 
  ())

(defclass unix-socket (socket) 
  ((family :initform sockint::af-local))
  (:default-initargs :type :stream))

(defmethod make-sockaddr-for ((socket unix-socket) &optional sockaddr &rest address)
  (apply '%sockaddr sockaddr address))

;;; NETLINK
(defclass netlink-socket (socket)
  ((family :initform af-netlink))
  (:documentation "Class representing NETLINK local sockets.")
  (:default-initargs :type :datagram))

(defmethod size-of-sockaddr ((self netlink-socket))
  io/socket::+size-of-sockaddr-nl+)

(defmethod make-sockaddr-for ((self netlink-socket) &optional sockaddr &rest address)
  (let ((sockaddr (or sockaddr (make-alien sockaddr-nl))))
    (destructuring-bind (&optional pid groups) address
      (setf (slot sockaddr 'io/socket::nl-family) af-netlink)
      (when pid (setf (slot sockaddr 'io/socket::nl-pid) pid))
      ;; TODO 2026-03-09: parse groups
      (when groups (setf (slot sockaddr 'io/socket::nl-groups) groups)))
    (values sockaddr io/socket::+size-of-sockaddr-nl+)))

(defmethod free-sockaddr-for ((socket netlink-socket) sockaddr)
  (sb-alien:free-alien sockaddr))

(defmethod bits-of-sockaddr ((socket netlink-socket) sockaddr &optional size)
  "Return the PID of the local socket address SOCKADDR. 0 indicates the kernel's address."
  (declare (ignore size))
  (values (slot sockaddr 'io/socket::nl-pid) (slot sockaddr 'io/socket::nl-groups)))
