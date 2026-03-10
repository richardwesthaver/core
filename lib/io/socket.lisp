;;; io/socket.lisp --- IO Sockets

;; 

;;; Code:
(in-package :io/socket)

(deftype ipv4-array () '(simple-array octet 4))
(deftype ipv6-array () '(simple-array (unsigned-byte 16) 8))

(defun integer-to-dotted (integer)
  "Convert an (UNSIGNED-BYTE 32) IPv4 address to a dotted string."
  (check-type integer (unsigned-byte 32) "an '(unsigned-byte 32)")
  (let ((*print-pretty* nil) (*print-base* 10))
    (format nil "~A.~A.~A.~A"
            (ldb (byte 8 24) integer)
            (ldb (byte 8 16) integer)
            (ldb (byte 8 8) integer)
            (ldb (byte 8 0) integer))))

(defun dotted-to-vector (address)
  "Convert a dotted IPv4 address to a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) 4)."
  (check-type address string "a string")
  (let ((addr (make-array 4 :element-type 'octet :initial-element 0))
        (split (split-sequence #\. address :count 5)))
    (flet ((set-array-value (index str)
             (setf (aref addr index)
                   (ensure-integer str :type 'octet))))
      (let ((len (length split)))
        (unless (<= 1 len 4)
          (error 'parse-error))
        (set-array-value 3 (nth (1- len) split))
        (loop for n in split
              for index below (1- len)
              do (set-array-value index n))))
    (values addr)))

(defun vector-to-dotted (vector)
  "Convert an 4-element vector to a dotted string."
  (coercef vector 'ipv4-array)
  (let ((*print-pretty* nil) (*print-base* 10))
    (with-output-to-string (s)
      (princ (aref vector 0) s) (princ #\. s)
      (princ (aref vector 1) s) (princ #\. s)
      (princ (aref vector 2) s) (princ #\. s)
      (princ (aref vector 3) s))))

(defun check-timeval (buffer size)
  (assert (= size #.(alien-size timeval :bytes)))
  buffer)

;;timevals
(defmacro define-socket-option-timeval (name level number &optional features (info ""))
  `(sb-bsd-sockets::define-socket-option ,name nil ,level ,number
     std/alien:timeval nil check-timeval addr ,features ,info))

(define-socket-option-timeval
    sockopt-receive-timeout sockint::sol-socket sockint::so-rcvtimeo)

(define-socket-option-timeval
    sockopt-send-timeout sockint::sol-socket sockint::so-sndtimeo)

;; linger
(define-alien-type linger 
  (struct linger
          (onoff int) ;; Nonzero to linger on close
          (linger int)))  ;; Time to linger

(defun check-linger (buffer size)
  (assert (= size #.(sb-alien:alien-size linger :bytes)))
  buffer)

(sb-bsd-sockets::define-socket-option sockopt-linger nil sockint::sol-socket sockint::so-linger
  linger nil check-linger sb-alien:addr)

(let ((so-reuseport 15))
  ;; [[file:/usr/include/asm-generic/socket.h::define SO_REUSEPORT 15][sys/socket.h]]
  (sb-bsd-sockets::define-socket-option-bool sockopt-reuse-port sockint::sol-socket so-reuseport))

;; ucre
(define-alien-type ucre
    (struct ucre
            (pid int)
            (uid int)
            (gid int)))

(defun check-ucre (buffer size)
  (assert (= size #.(sb-alien:alien-size ucre :bytes)))
  buffer)

(sb-bsd-sockets::define-socket-option sockopt-peer-cred nil sockint::sol-socket sockint::so-linger
  ucre nil check-ucre sb-alien:addr)

;;; NETLINK
(defconstant af-netlink sockint::af-route)

;; (defconstant +netlink-max+ 32)

(define-alien-type sockaddr-nl
    (struct sockaddr-nl
      (nl-family int) ;; af-netlink
      (nl-pad unsigned-short) ;; 0
      (nl-pid (unsigned 32)) ;; port ID
      (nl-groups (unsigned 32)))) ;; multicast groups mask

(std/alien:define-alien-enum (netlink-proto :type int)
  :route 0
  :unused 1
  :usersock 2
  ;; :firewall 3
  :sock-diag 4
  :inet-diag :sock-diag
  ;; :nflog 5
  :xfrm 6
  :selinux 7
  :iscsi 8
  :audit 9
  :fib-lookup 10
  :connector 11
  :netfilter 12
  ;; :ip6-fw 13
  ;; :dnrtmsg 14
  :kobject-uevent 15
  :generic 16
  :scsitransport 18
  :ecryptfs 19
  :rdma 20
  :crypto 21
  :smc 22)

(define-alien-type nlmsghdr
    (struct nlmsghdr
      (len (unsigned 32))
      (type (unsigned 16))
      (flags (unsigned 16))
      (seq (unsigned 32))
      (pid (unsigned 32))))

(std/alien:define-alien-enum (nlm-f :type (unsigned 16))
  :request #x01
  :multi #x02
  :ack #x04
  :echo #x08
  :dump-intr #x10
  :dump-filtered #x20
  ;; get request
  :root #x100
  :match #x200
  :atomic #x400
  :dump (logior #x100 #x200)
  ;; new request
  :replace #x100
  :excl #x200
  :create #x400
  :append #x800
  ;; delete request
  :nonrec #x100
  :bulk #x200
  ;; ack
  :capped #x100
  :ack-tlvs #x200)

(std/alien:define-alien-enum (nlmsg)
  :noop #x1
  :error #x2
  :done #x3
  :overrun #x4
  :min-type #x10)

(define-alien-type nlmsgerr
    (struct nlmsgerr
      (error int)
      (msg nlmsghdr)))

(std/alien:define-alien-enum (nlmsgerr-attr)
  :unused 0
  :msg 1
  :offs 2
  :cookie 3
  :policy 4
  :miss-type 5
  :miss-nest 6)
;; (:max 7)

(std/alien:define-alien-enum (netlink-attribute-type)
  :invalid 0
  :flag 1
  :u8 2
  :u16 3
  :u32 4
  :u64 5
  :s8 6
  :s16 7
  :s32 8
  :s64 9
  :binary 10
  :string 11
  :nul-string 12
  :nested 13
  :nested-array 14
  :bitfield32 15
  :sint 16
  :uint 17)

(std/alien:define-alien-enum (netlink-policy-type-attr)
  :unspec 0
  :type 1
  :min-value-s 2
  :max-value-s 3
  :min-value-u 4
  :max-value-u 5
  :min-length 6
  :max-length 7
  :policy-idx 8
  :policy-maxtype 9
  :bitfield32-mask 10
  :pad 11
  :mask 12)
;; (:max 12)

(defconstant +size-of-sockaddr-nl+ (sb-alien::alien-size sockaddr-nl))

;;; ICMP
(define-alien-type ip-header
    (struct ip-header
      (ver-ihl  (unsigned 8))
      (tos      (unsigned 8))
      (length   (unsigned 16))
      (id       (unsigned 16))
      (offset   (unsigned 16))
      (ttl      (unsigned 8))
      (protocol (unsigned 8))
      (checksum (unsigned 16))
      (saddr    (unsigned 32))
      (daddr    (unsigned 32))))

(define-alien-type icmp-header
    (struct icmp-header
      (type     (unsigned 8))
      (code     (unsigned 8))
      (checksum (unsigned 16))
      (quench   (unsigned 32))))

(defun write-ip-header (ip-header total-length target-ip)
  (std/alien:with-alien-slots (ver-ihl length id offset ttl protocol daddr) ip-header
    (setf ver-ihl  #x45       ; Version 4, header length 5 words(20 bytes)
          length   total-length
          offset   #b01000000 ; Don't fragment
          ttl      64
          protocol sockint::ipproto_icmp
          daddr    (io/swap-bytes:htonl target-ip))))

(defun compute-icmp-checksum (icmp-header packet-size)
  (let* ((sum1
           (loop :for offset :from 0 :below (/ packet-size 2)
                 :sum (std:sap-ref icmp-header 'unsigned-short offset)))
         (sum2 (+ (ash sum1 -16)
                  (logand sum1 #xFFFF))))
    (logand #xFFFF (lognot (+ sum2 (ash sum2 -16))))))

(defun write-icmp-header (icmp-header packet-size id seqno)
  (std:with-alien-slots (type quench checksum) icmp-header
    (let ((new-quench
            (+ (ash id 16) seqno)))
      (setf type     icmp-echo-request
            quench   (htonl new-quench))
      (setf checksum (compute-icmp-checksum icmp-header packet-size)))))

#+todo
(defun ping (target &key (id #xFF) (seqno 1))
  (with-open-socket (socket :address-family :ipv4 :type :raw :protocol sockint::ipproto_icmp
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
