;;; lan-party.lisp --- Simulate a complex network of UDP nodes

;; 

;;; Code:
(in-package :std-user)

(defpkg :bench/lan-party
  (:use :cl :std :net/srv/udp :log :json :obj :rdb :net :graph)
  (:export :lan-party :lan-party-config :lan-node :emacs-lan-node))

(in-package :bench/lan-party)

;; Config
(defconfig lan-party-config (ast) 
  ((emacs-nodes)
  (lan-nodes)
  (port-range)))

;;; LAN Node
(defservice lan-node (udp-service rdb-service worker vertex) ()
  (:documentation "LAN Nodes bind to a single UDP socket and implement a simple message-passing
protocol.")
  (:default-initargs :port (cons 42000 44000)))

;;; Emacs Node
(defservice emacs-lan-node (lan-node) ()
  (:documentation "A LAN Node which controls an Emacs instance."))

;;; LAN Party
(defclass lan-party (thread-pool)
  ((admin :initarg :admin :reader lan-party-admin :type supervisor))
  (:documentation "A LAN party simply wraps a thread-pool which manages nodes.")
  (:default-initargs :name (gensymify :lan)))

(defmethod initialize-instance :before ((self lan-party) &rest args)
  (declare (ignore args))
  (load-database-backend :rdb))

;;; Main
(defun start-lan-party (node-count)
  "Start the lan-party. NODE-COUNT Nodes are initialized with WORKER-COUNT
  workers assigned to them from a shared thread-pool."
  (mumble "starting lan party with ~D nodes." node-count)
  (setq *thread-pool* (make-thread-pool node-count :name :lan-party :worker-class 'lan-node :class 'lan-party))
  (start *thread-pool*))

(defmethod start ((self (eql :lan-party)))
  (start-lan-party 8))
