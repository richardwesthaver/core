;;; socket.lisp --- High-level Socket API

;; based on IOLib's make-socket.lisp

;;; Code:
(in-package :net/core)

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

