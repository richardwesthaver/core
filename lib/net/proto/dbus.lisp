;;; dbus.lisp --- DBUS Protocol

;; DBUS Protocol Definitions

;;; Code:
(in-package :net/proto/dbus)

(defclass dbus-connection (connection) ()
  (:documentation "A connection from a client to a DBUS server."))

(defgeneric supports-unix-fd-passing-p (connection)
  (:documentation "Return true if Unix file descriptors can be passed
over the connection, and false otherwise."))

(defgeneric receive-line (connection)
  (:documentation "Read a line of text from the server and return it as
a string.  The operation blocks until a whole line can be read.  The
string will not contain newline characters."))

(defgeneric send-line (line connection)
  (:documentation "Send a line of text, represented by a string, to
the server.  The operation will force (but not finish) output before
returning.  The string should not contain any newline characters."))
