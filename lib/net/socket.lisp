;;; socket.lisp --- High-level Socket API

;; based on IOLib's make-socket.lisp

;;; Code:
(in-package :net/core)

;; client-socket = active-socket
;; server-socket = passive-socket
(defun make-socket (&rest args &key family type protocol connect ipv6 &allow-other-keys)
  (check-type family (member :internet :inet :unix :local :ipv4 :ipv6 :netlink)
              "one of :INTERNET(or :INET), :LOCAL(or :FILE, :UNIX), :IPV4, :IPV6 or :NETLINK")
  (check-type type (member :stream :datagram :raw) "either :STREAM, :DATAGRAM or :RAW")
  (check-type connect (member :active :passive) "either :ACTIVE or :PASSIVE")
  (let ((args (remove-from-plist args :family :type :protocol :connect :ipv6)))
    (when (eql :ipv4 family) (setf ipv6 nil))
    (let ((*ipv6* ipv6))
      (when (or (eql :internet family)
                (eql :inet family))
        (setf family default-inet-address-family)))))

#+todo
(define-compiler-macro make-socket (&whole form &environment env &rest args
                                    &key (family :internet) (type :stream) (protocol :default)
                                    (connect :active) (ipv6 '*ipv6* ipv6p) &allow-other-keys)
  (when (eql :file family) (setf family :local))
  (cond
    ((and (constantp family env) (constantp type env) (constantp connect env))
     (check-type family (member :internet :local :ipv4 :ipv6 :netlink)
                 "one of :INTERNET, :LOCAL(or :FILE), :IPV4, :IPV6 or :NETLINK")
     (check-type type (member :stream :datagram :raw) "either :STREAM, :DATAGRAM or :RAW")
     (check-type connect (member :active :passive) "either :ACTIVE or :PASSIVE")
     (let* ((family (if (member family '(:ipv4 :ipv6)) :internet family))
            (lower-function (make-first-level-name family type connect))
            (args (remove-from-plist args :family :type :protocol :connect :ipv6)))
       (case family
         (:internet (setf family '+default-inet-address-family+))
         (:ipv4     (setf ipv6 nil ipv6p t)))
       (let ((expansion `(,lower-function (list ,@args) ,family ,protocol)))
         (if ipv6p `(let ((*ipv6* ,ipv6)) ,expansion) expansion))))
    (t form)))

(defmacro with-open-socket ((var &rest args) &body body)
  `(with-open-stream (,var (make-socket ,@args)) ,@body))

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
