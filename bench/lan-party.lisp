;;; lan-party.lisp --- Simulate a complex network of UDP nodes

;; 

;;; Code:
(in-package :std-user)
(defpkg :bench/lan-party
  (:use :cl :std :net/srv/udp :log :json :obj))
(in-package :bench/lan-party)
(defvar *default-config* (make-config :udp))

(defclass lan-node (udp-service) ())

(defun start-lan-party (node-count worker-count &key (port-range '(42000 . 44000)))
  "Start the lan-party. NODE-COUNT Nodes are initialized with WORKER-COUNT
  workers assigned to them from a shared thread-pool."
  (mumble "starting lan party with ~D nodes and ~D total workers." 
          node-count (* node-count worker-count))
  (mumble "port range: ~A" port-range)
  (let ((nodes (make-array node-count 
                           :element-type 'lan-node 
                           :initial-element (make-instance 'lan-node :port port-range)))
        (pool (make-thread-pool (* worker-count node-count) :name :lan-party)))
    (with-thread-pool (:lan-party)
      (values nodes pool))))
