;;; db.lisp --- Database Logger

;; Log messages to a database

;;; Code:
(in-package :log)

(defclass db-sink (sink database) ())

(defmethod msg ((self db-sink) (msg log-message))
  (put-key (column self (level msg)) (timestamp msg) (content msg)))

(defmethod msg ((self db-sink) (msg simple-message))
  (put-key (column self (level msg)) (timestamp msg) (cons (tags msg) (content msg))))

(defclass database-logger (database logger) ()
  (:documentation "A LOGGER which writes messages to a DATABASE."))
