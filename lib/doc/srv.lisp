;;; lib/doc/srv.lisp --- Documentation Services

;;

;;; Code:
(in-package :doc)

;; TODO 2026-06-20: don't forget about indexing
(defclass doc-service (service)
  ()
  (:documentation "Base class for documentation services."))

(defclass doc-request (service-request) ())

(defclass doc-response (service-response) ())
