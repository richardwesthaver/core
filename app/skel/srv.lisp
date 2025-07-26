;;; srv.lisp --- Skel Service

;; 

;;; Code:
(in-package :skel/srv)

(defclass sk-request (request) ())
(defclass sk-response (response) ())

(defclass sk-service (skel service) ()
  (:documentation "Base class for SKEL services.")
  (:default-initargs
   :request-class 'sk-request
   :response-class 'sk-response))

(defclass sk-engine (multi-threaded-engine thread-pool) ())

(defmethod print-object ((self sk-service) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (id:id self))))
