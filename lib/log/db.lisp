;;; db.lisp --- Database Logger

;; Log messages to a database

;;; Code:
(in-package :log)

(defclass db-sink (sink database) ())

(defmethod msg ((self db-sink) (msg log-message))
  (insert-key self (timestamp-to-octets (timestamp msg)) (content msg) :column (level msg)))

(defmethod msg ((self db-sink) (msg simple-message))
  (insert-key self (timestamp-to-octets (timestamp msg)) (cons (tags msg) (content msg)) :column (level msg)))

(defclass database-logger (database logger) ()
  (:documentation "A LOGGER which writes messages to a DATABASE."))
