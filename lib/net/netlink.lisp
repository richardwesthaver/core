;;; netlink.lisp --- Netlink Sockets

;; 

;;; Code:
(in-package :net/core)

(defclass netlink-socket (socket)
  ((family :initform af-netlink))
  (:documentation "Class representing NETLINK local sockets.")
  (:default-initargs :type :datagram))

(defmethod size-of-sockaddr ((self netlink-socket))
  io/socket::+size-of-sockaddr-nl+)

(defmethod make-sockaddr-for ((self netlink-socket) &optional sockaddr &rest address)
  (let ((sockaddr (or sockaddr (make-alien sockaddr-nl))))
    (destructuring-bind (&optional pid groups) address
      (setf (slot sockaddr 'nl-family) af-netlink)
      (when pid (setf (slot sockaddr 'nl-pid) pid))
      (when groups (setf (slot sockaddr 'nl-groups) groups)))
    (values sockaddr io/socket::+size-of-sockaddr-nl+)))

(defmethod free-sockaddr-for ((socket netlink-socket) sockaddr)
  (sb-alien:free-alien sockaddr))

(defmethod bits-of-sockaddr ((socket netlink-socket) sockaddr &optional size)
  "Return the PID of the local socket address SOCKADDR. 0 indicates the kernel's address."
  (declare (ignore size))
  (slot sockaddr 'nl-pid))
