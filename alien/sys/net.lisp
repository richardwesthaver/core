;;; net.lisp --- Linux networking

;; 

;;; Code:
(in-package :sys)

(defsyscall if-nametoindex unsigned-int (ifname c-string))
(defsyscall if-indextoname c-string (ifindex unsigned-int) (ifname (array char 16) :out))
(defsyscall if-nameindex (* if-nameindex))
(defsyscall if-freenameindex void (ptr (* if-nameindex)))
