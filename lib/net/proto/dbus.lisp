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

;;; Addresses
(define-class-map
  :class server-address
  :map *server-address-classes*
  :find find-server-address-class)

(defclass standard-server-address (server-address)
  ((transport-name :initarg :transport-name :reader server-address-transport-name)
   (properties :initarg :properties :reader server-address-properties))
  (:documentation "Represents a standard server address with a
transport name and a table of properties."))

(defmethod connect :around ((server-address standard-server-address) &key (if-failed :error))
  (with-if-failed-handler if-failed
    (call-next-method)))

(defmethod connect ((addresses list) &key (if-failed :error) event-base)
  (with-if-failed-handler if-failed
    (or (some (lambda (address)
                (connect address :if-failed nil :event-base event-base))
              addresses)
        (error "No server addresses left to try to open."))))

(defmethod server-address-property (name (server-address standard-server-address)
                                    &key (if-does-not-exist :error))
  (or (gethash name (server-address-properties server-address))
      (missing-entry name if-does-not-exist)))

(defclass generic-server-address (standard-server-address)
  ()
  (:documentation "Represents a server address whose transport is not
supported by the DBUS system."))

(defmethod connect ((address generic-server-address) &key)
  (error "Unsupported transport mechanism for ~S." address))

(defun parse-server-addresses-from-stream (in)
  "Parse unescaped server addresses text from a character stream and
return a list of server addresses."
  (let ((server-addresses '())
        (token (make-string-output-stream))
        (current-server-address '())
        (char nil))
    (labels ((consume ()
               (or (setf char (read-char in nil nil))
                   (finish)))
             (finish ()
               (finish-token)
               (finish-server-address)
               (return-from parse-server-addresses-from-stream
                 (nreverse server-addresses)))
             (finish-token (&optional ignore-empty)
               (let ((string (get-output-stream-string token)))
                 (when (or (plusp (length string))
                           (not ignore-empty))
                   (push string current-server-address))))
             (finish-server-address (&optional ignore-empty)
               (finish-token ignore-empty)
               (when current-server-address
                 (destructuring-bind (type &rest plist)
                     (nreverse current-server-address)
                   (push (make-instance
                          (or (find-server-address-class type :if-does-not-exist nil)
                              'generic-server-address)
                          :transport-name type
                          :properties (plist-hash-table plist :test 'equal))
                         server-addresses))
                 (setf current-server-address '())))
             (add-to-token ()
               (write-char char token)))
      (tagbody
       transport
         (case (consume)
           (#\: (finish-token) (go key))
           (t (add-to-token) (go transport)))
       key
         (case (consume)
           (#\; (finish-server-address t) (go transport))
           (#\= (finish-token) (go value))
           (t (add-to-token) (go key)))
       value
         (case (consume)
           (#\, (finish-token) (go key))
           (#\; (finish-server-address) (go transport))
           (t (add-to-token) (go value)))))))

(defun unescape-server-addresses-string (string)
  "Unescape a server addresses string per the DBUS specification's
escaping rules and return the unescaped string.  The string returned
may be the same as the string supplied if no unescaping is needed."
  (let ((escapes (count #\% string)))
    (if (zerop escapes)
        string
        (let ((octets (make-octets (- (length string) (* 2 escapes))
                                   :fill-pointer 0)))
          (with-input-from-string (in string)
            (loop for char = (read-char in nil nil)
                  while char do
                  (vector-push
                   (if (char= #\% char)
                       (logior (ash (digit-char-p (read-char in) 16) 4)
                               (digit-char-p (read-char in) 16))
                       (char-code char))
                   octets)))
          (sb-ext:octets-to-string octets :external-format :utf-8)))))

(defun parse-server-addresses-string (string)
  "Parse a (possibly escaped) server addresses string into a list of
server addresses."
  (with-input-from-string (in (unescape-server-addresses-string string))
    (parse-server-addresses-from-stream in)))

(defun session-server-addresses ()
  "Return a list of server addresses for the current session."
  (when-let ((string (sb-posix:getenv "DBUS_SESSION_BUS_ADDRESS")))
    (parse-server-addresses-string string)))

(defun system-server-addresses ()
  "Return a list of server addresses for the current system."
  (parse-server-addresses-string
   (or (sb-posix:getenv "DBUS_SYSTEM_BUS_ADDRESS")
       "unix:path=/var/run/dbus/system_bus_socket")))

(defclass unix-server-address (standard-server-address)
  ((socket-address :reader socket-address))
  (:documentation "Represents a DBUS server address with Unix Domain
Sockets for transport."))

(setf (find-server-address-class "unix") 'unix-server-address)

#+todo
(defmethod shared-initialize :after ((address unix-server-address) slot-names &rest initargs)
  (declare (ignore initargs slot-names))
  (let ((abstract (server-address-property "abstract" address :if-does-not-exist nil))
        (path (server-address-property "path" address :if-does-not-exist nil)))
    (with-slots (socket-address) address
      (setf socket-address
            (ensure-address (or abstract path)
                            :family :local
                            :abstract (if abstract t nil))))))
