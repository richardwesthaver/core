(in-package :net/util)

(defun get-address-by-name (name)
  (multiple-value-bind (host4 host6)
      (get-host-by-name name)
    (let ((addr4 (when host4
                   (car (sb-bsd-sockets::host-ent-addresses host4))))
          (addr6 (when host6
                   (car (sb-bsd-sockets::host-ent-addresses host6)))))
      (values addr4 addr6))))

;; (get-address-by-name "localhost")
