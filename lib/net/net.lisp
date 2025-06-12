;;; net.lisp --- Network Top-level

;; 

;;; Code:
(pkg:defpkg :net
  (:use :cl :std)
  (:use-reexport 
   :net/core 
   :net/tcp 
   :net/udp
   :net/srv
   :net/codec/dns 
   :net/codec/osc 
   :net/codec/tlv
   :net/codec/http
   :net/proto/dns
   :net/proto/ssh
   :net/proto/http))

(pkg:defpkg :net-user
  (:use :cl :std :std-user :net :obj))

(in-package :net)
(when (sb-int:featurep :swank)
  #+quicklisp (ql:quickload '(:swank-client))
  (use-package :net/proto/swank)
  (use-package :net/proto/crew))
  
