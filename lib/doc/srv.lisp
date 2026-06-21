;;; lib/doc/srv.lisp --- Documentation Services

;;

;;; Code:
(in-package :doc)

;; TODO 2026-06-20: don't forget about indexing
(defclass publisher-engine (engine)
  ()
  (:documentation "A dedicated engine for publishing documentation."))

(defclass doc-service (service)
  ()
  (:documentation "Base class for documentation services."))

(defclass doc-request (request) ())

(defclass doc-response (response) ())
