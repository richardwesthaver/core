(in-package :net/util)

(defvar *localhost* #(127 0 0 1))

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

(defun find-port (&key (min 32000) (max 48000) (host *localhost*))
  "Return the first available port in a range of port numbers."
  (loop :for port :from min :to max :when (port-open-p port :host host) :return port))

;; (get-address-by-name "localhost")
