;;; lan-party.lisp --- Simulate a complex network of UDP nodes

;; 

;;; Code:
(in-package :std-user)
(defpkg :bench/lan-party
  (:use :cl :std :net/srv/udp :log :json :obj :rdb :net))
(in-package :bench/lan-party)

;; Config
(defconfig lan-party-config (ast) ())

;;; LAN Node
(defservice lan-node (udp-service rdb-service worker) ()
  (:documentation "LAN Nodes bind to a single UDP socket and implement a simple message-passing
protocol.")
  (:default-initargs :port (cons 42000 44000)))

;;; Emacs Node
(defservice emacs-lan-node (lan-node) ()
  (:documentation "A LAN Node which controls an Emacs instance."))

;;; LAN Party
(defclass lan-party (thread-pool)
  ((logger :initarg :logger :accessor logger))
  (:documentation "A LAN party simply wraps a thread-pool which manages nodes."))

(defmethod initialize-instance :before ((self lan-party) &rest args)
  (declare (ignore args))
  (load-database-backend :rdb))

;;; Main
(defun start-lan-party (node-count)
  "Start the lan-party. NODE-COUNT Nodes are initialized with WORKER-COUNT
  workers assigned to them from a shared thread-pool."
  (mumble "starting lan party with ~D nodes." node-count)
  (make-thread-pool node-count :name :lan-party :worker-class 'lan-node)
  (with-thread-pool (:lan-party)
    (start *thread-pool*)))
