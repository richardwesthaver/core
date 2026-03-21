;;; addr.lisp --- Network Addresses

;; 

;;; Code:
(in-package :net/core)

;; TODO 2026-03-21: URLs

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

(defun inet-address-network-portion (address netmask)
  "Apply network netmask NETMASK to ADDRESS in order to calculate the
network part of ADDRESS."
  (let ((v (make-array 4 :element-type 'octet)))
    (dotimes (i 4)
      (setf (aref v i)
            (logand (aref address i)
                    (aref netmask i))))
    v))

(defun inet-address-host-portion (address netmask)
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
        (inet-address-network-portion address netmask)))

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
