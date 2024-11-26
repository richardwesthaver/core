;;; lan-party.lisp --- Simulate a complex network of UDP nodes

;; 

;;; Code:
(in-package :std-user)
(defpkg :bench/net/lan-party
  (:nicknames :bench/lan-party)
  (:use :cl :std :net :log :json :obj))
(in-package :bench/net/lan-party)
