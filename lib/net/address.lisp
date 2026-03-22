;;; address.lisp --- Network Addresses

;; 

;;; Code:
(in-package :net/core)

;; TODO 2026-03-21: URLs
;; TODO 2026-03-21: ipv6/graph

(defun make-netmask (&key cidr class)
  "Create a subnet mask by specifying either its class(:A, :B or :C) or
a CIDR suffix(a number between 0 and 32)."
  (assert (or cidr class) (cidr class) "You must either specify a CIDR or a network class.")
  (cond
    (cidr (check-type cidr (mod 33) "a number between 0 and 32"))
    (class (check-type class (member :a :b :c)
                       "a valid network class - one of :A, :B or :C")
           (setf cidr (case class (:a 8) (:b 16) (:c 24)))))
  (let ((mask #xFFFFFFFF))
    (declare (type (unsigned-byte 32) mask))
    (setf (ldb (byte (- 32 cidr) 0) mask) 0)
    (make-inet-address (integer-to-dotted mask))))

(defun address-network-portion (address netmask)
  "Apply network netmask NETMASK to ADDRESS in order to calculate the
network part of ADDRESS."
  (let ((v (make-array 4 :element-type 'octet)))
    (dotimes (i 4)
      (setf (aref v i)
            (logand (aref address i)
                    (aref netmask i))))
    v))

(defun address-host-portion (address netmask)
  "Apply network netmask NETMASK to ADDRESS in order to calculate the
host part of ADDRESS."
  (let ((v (make-array 4 :element-type 'octet)))
      (dotimes (i 4)
        (setf (aref v i)
              (logand (aref address i)
                      (logxor (aref netmask i) 255))))
    v))

(defstruct ipv4-network
  address
  mask
  cidr)

(defmethod address ((self ipv4-network)) (ipv4-network-address self))

(definline count-trailing-zeroes/32 (n)
  (declare (optimize speed) ((unsigned-byte 32) n))
  (1- (integer-length (logand n (- n)))))

(defun cidr-subnet-zeroes (netmask) (count-trailing-zeroes/32 (octets-to-integer netmask)))

(defmethod initialize-instance :after ((network ipv4-network)
                                       &key address netmask)
  (check-type address ip-address "an Ipv4 address")
  (check-type netmask ip-address "an Ipv4 netmask")
  (setf (ipv4-network-cidr network) (- 32 (cidr-subnet-zeroes netmask)))
  (setf (ipv4-network-mask network) netmask)
  (setf (ipv4-network-address network)
        (address-network-portion address netmask)))

(defmethod print-object ((network ipv4-network) stream)
  (let ((namestring
         (format nil "~A/~A"
                 (vector-to-dotted (ipv4-network-address network))
                 (ipv4-network-cidr network))))
    (if (or *print-readably* *print-escape*)
        (format stream "#/~S/~A" 'net namestring)
        (write-string namestring stream))))

(defmethod equiv ((net1 ipv4-network) (net2 ipv4-network))
  "Returns T if the addresses and the netmasks of the
two arguments are respectively equiv."
  (and (equiv (ipv4-network-address net1) (ipv4-network-address net2))
       (equiv (ipv4-network-mask net1) (ipv4-network-mask net2))))

(defun address-in-network-p (address network)
  "Return T if ADDRESS is part of the subnet specified by NETWORK."
  (equiv (address-network-portion address (ipv4-network-mask network))
         (address network)))

(defun addresses-in-same-network-p (address1 address2 network)
  "Return T if ADDRESS1 and ADDRESS2 are both part part of the
subnet specified by NETWORK."
  (let ((address1-network (address-network-portion address1 (ipv4-network-mask network)))
        (address2-network (address-network-portion address2 (ipv4-network-mask network))))
    (and (equiv address1-network (address network))
         (equiv address2-network (address network)))))

(defun address-network-class (address)
  "Return the network class of ADDRESS: one of :A, :B, :C, :D or :E ."
  (let ((octet (aref address 0)))
    (cond
      ((= #b0000 (ldb (byte 1 7) octet)) :a) ;   0.0.0.0 - 127.255.255.255
      ((= #b0010 (ldb (byte 2 6) octet)) :b) ; 128.0.0.0 - 191.255.255.255
      ((= #b0110 (ldb (byte 3 5) octet)) :c) ; 192.0.0.0 - 223.255.255.255
      ((= #b1110 (ldb (byte 4 4) octet)) :d) ; 224.0.0.0 - 239.255.255.255
      ((= #b1111 (ldb (byte 4 4) octet)) :e)))) ; 240.0.0.0 - 255.255.255.255

(defun address-private-p (address)
  "Returns T if ADDRESS is in a private network range.
Private IPv4 networks are 10.0.0.0/8, 172.16.0.0/12 and 192.168.0.0/16.
See http://en.wikipedia.org/wiki/Private_network for details."
  (let* ((first (aref address 0))
         (second (aref address 1)))
    (values (or (= first 10)
                (and (= first 172)
                     (<= 16 second 31))
                (and (= first 192)
                     (= second 168)))
              (address-network-class address))))
