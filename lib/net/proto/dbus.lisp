;;; dbus.lisp --- DBUS Protocol

;; DBUS Protocol Definitions

;;; Code:
(in-package :net/proto/dbus)
;;; Conditions
(define-condition dbus-error (error)
  ())

(define-condition dbus-auth-error (dbus-error)
  ((command :initarg :command)
   (argument :initarg :argument))
  (:report (lambda (condition stream)
             (format stream "Authentication error, command ~S with argument ~S."
                     (slot-value condition 'command)
                     (slot-value condition 'argument)))))

(define-condition dbus-method-error (dbus-error)
  ((arguments :initarg :arguments))
  (:report (lambda (condition stream)
             (format stream "Method error: ~S."
                     (let ((all-args (slot-value condition 'arguments))
                           (first-arg (first (slot-value condition 'arguments))))
                       (if (stringp first-arg)
                           first-arg
                           all-args))))))

;;; Addresses
(defclass server-address () ())

(define-class-map
  :class server-address
  :map *server-address-classes*
  :find find-server-address-class)

(defclass standard-server-address (server-address)
  ((transport-name :initarg :transport-name :reader server-address-transport-name)
   (properties :initarg :properties :reader server-address-properties))
  (:documentation "Represents a standard server address with a
transport name and a table of properties."))

(defmethod connect :around (self (server-address standard-server-address) &key (if-failed :error))
  (with-if-failed-handler if-failed
    (call-next-method)))

(defmethod connect (event-base (addresses list) &key (if-failed :error))
  (with-if-failed-handler if-failed
    (or (some (lambda (address) (connect event-base address :if-failed nil))
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

(defmethod connect (event-base (address generic-server-address) &key)
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
  ((address :reader address))
  (:documentation "Represents a DBUS server address with Unix Domain
Sockets for transport."))

(setf (find-server-address-class "unix") 'unix-server-address)

(defmethod shared-initialize :after ((address unix-server-address) slot-names &rest initargs)
  (declare (ignore initargs slot-names))
  (let ((abstract (server-address-property "abstract" address :if-does-not-exist nil))
        (path (server-address-property "path" address :if-does-not-exist nil)))
    (with-slots (address) address
      (setf address (or abstract path)))))

;;; Connections
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

(defclass standard-dbus-connection (connection id)
  ((address :initarg :address :reader address)
   (pending-messages :initform '() :accessor connection-pending-messages)
   (event-base :initarg :event-base :reader connection-event-base)
   (serial :initform 1)
   (supports-unix-fd-passing :initform nil :accessor supports-unix-fd-passing-p))
  (:default-initargs :id nil)
  (:documentation "Represents a standard DBUS connection."))

(defmethod (setf id) :before (new-id (connection standard-dbus-connection))
  (let ((old-id (id connection)))
    (when (and old-id (not (equal old-id new-id)))
      (cerror "Set new ID and continue."
              "A server ID is already assigned to this connection."))))

(defmethod next-id ((connection standard-dbus-connection))
  (with-slots (serial) connection
    (prog1 serial
      (setf serial
            (let ((x (logand (1+ serial) #xFFFFFFFF)))
              (if (zerop x) 1 x))))))

(defmethod drain-pending-messages ((connection standard-dbus-connection))
  (prog1 (nreverse (connection-pending-messages connection))
    (setf (connection-pending-messages connection) '())))

(defmethod wait-for-reply (serial (connection standard-dbus-connection))
  (loop
   (dolist (message (connection-pending-messages connection))
     (when (and (typep message '(or dbus-error-message dbus-method-return-message))
                (= serial (net/codec/dbus::message-reply-serial message)))
       (deletef (connection-pending-messages connection) message :count 1)
       (return-from wait-for-reply
         (values (net/codec/dbus::message-body message) message))))
    (io/mux:event-dispatch (connection-event-base connection) :oneshot t)))

(defun activate-io-handlers (connection)
  (set-io-handler
   (connection-event-base connection)
   (fd connection)
   :read
   (lambda (fd event error)
     (declare (ignore fd event))
     (when error
       (error "Connection I/O error: ~S." error))
     (loop for message = (receive-message-no-hang connection)
           while message
           do (push message (connection-pending-messages connection))))))

(defmethod supported-authenticators ((connection standard-dbus-connection))
  (send-authentication-command connection :auth)
  (mapcar (lambda (name)
            (make-instance
                (or (find-authenticator-class name :if-does-not-exist nil)
                    'generic-authentication-mechanism)
              :name name))
          (print (receive-authentication-response connection :expect :rejected))))

(defmethod authenticate :around (mechanisms (connection standard-dbus-connection) &key (if-failed :error))
  (with-if-failed-handler if-failed
    (when (call-next-method)
      (activate-io-handlers connection)
      t)))

(defmethod authenticate (mechanisms (connection standard-dbus-connection) &key (if-failed :error))
  (declare (ignore if-failed))
  (setf mechanisms (ensure-list mechanisms))
  (let (op arg mechanism)
    (flet ((send (command &rest args)
             (apply #'send-authentication-command connection command args))
           (receive ()
             (receive-authentication-response connection :as-string (authenticator-textual-p mechanism))))
      (tagbody
       initial
         (if (null mechanisms)
             (error "No more mechanisms to try.")
             (setf mechanism (pop mechanisms)))
         (multiple-value-setq (op arg) (authenticator-challenge mechanism :initial-response))
         (when (eq op :error)
           (go initial))
         (send :auth (name mechanism) arg)
         (ecase op
           (:ok (go waiting-for-ok))
           (:continue (go waiting-for-data)))
       waiting-for-data
         (multiple-value-setq (op arg) (receive))
         (case op
           (:data
            (multiple-value-setq (op arg) (authenticator-challenge mechanism arg))
            (ecase op
              (:continue (send :data arg) (go waiting-for-data))
              (:ok (send :data arg) (go waiting-for-ok))
              (:error (if arg (send :error arg) (send :error)) (go waiting-for-data))))
           (:rejected (go initial))
           (:error (send :cancel) (go waiting-for-reject))
           (:ok (go got-ok))
           (t (send :error) (go waiting-for-data)))
       waiting-for-ok
         (multiple-value-setq (op arg) (receive))
         (case op
           (:ok (go got-ok))
           (:reject (go initial))
           ((:data :error) (send :cancel) (go waiting-for-reject))
           (t (send :error) (go waiting-for-ok)))
       waiting-for-reject
         (multiple-value-setq (op arg) (receive))
         (case op
           (:reject (go initial))
           (t (error 'authentication-error :command op :argument arg)))
       got-ok
         (setf (id connection) arg)
         (send :negotiate-unix-fd)
         (go wait-for-unix-fd-passing-agreement)
       wait-for-unix-fd-passing-agreement
         (multiple-value-setq (op arg) (receive))
         (case op
           (:error
            (setf (supports-unix-fd-passing-p connection) nil))
           (:agree-unix-fd
            (setf (supports-unix-fd-passing-p connection) t))
           (t (error 'authentication-error :command op :argument arg)))
         (send :begin)
         (go authenticated)
       authenticated)))
  t)

;;;; Socket-based connection mixin
(defclass dbus-socket-connection-mixin (connection wrapped-socket)
  ((socket :initarg :socket :reader connection-socket)))

(defun open-socket-connection (family address)
  ;; iolib: make-socket connect
  (let ((socket (make-socket 
                 :family family
                 :class :socket
                 :external-format '(:utf-8 :newline :crlf))))
    (unwind-protect
         (progn
           (socket-connect socket address)
           (with-socket-stream (s socket :input t :output t :element-type 'character :external-format '(:utf-8 :newline :crlf))
             (write-char #\Nul s)
             (force-output s)
             (prog1 socket
               (setf socket nil))))
      (when socket
        (socket-close socket)))))

(defmethod fd ((connection dbus-socket-connection-mixin))
  (socket-file-descriptor (connection-socket connection)))

(defmethod disconnect ((connection dbus-socket-connection-mixin))
  (socket-close (connection-socket connection)))

(defmethod receive-message-no-hang ((connection dbus-socket-connection-mixin))
  (decode-dbus-message (connection-socket connection)))

(defmethod receive-line ((connection dbus-socket-connection-mixin))
  (with-socket-stream (s (print (connection-socket connection)) :input t :external-format '(:utf-8 :newline :crlf))
    (read-char s)
    (read-line s)))

(defmethod send-line (line (connection dbus-socket-connection-mixin))
  (with-socket-stream (s (connection-socket connection) :output t :external-format '(:utf-8 :newline :crlf))
    (write-line line s)
    (force-output s)))

(defmethod send-message (encoded-message (connection dbus-socket-connection-mixin))
  (write-sequence encoded-message (connection-socket connection))
  (force-output (connection-socket connection)))

;;;; Unix Connection
(defclass dbus-unix-connection (dbus-socket-connection-mixin standard-dbus-connection)
  ()
  (:documentation "Represents a connection to a DBUS server over Unix
Domain Sockets."))

(defmethod connect (event-base (address unix-server-address) &key (if-failed :error))
  (declare (ignore if-failed))
  (make-instance 'dbus-unix-connection
    :socket (open-socket-connection :local (address address))
                 :address address
                 :id (server-address-property "guid" address :if-does-not-exist nil)
                 :event-base event-base))

;;; Authentication
(define-class-map
  :class authenticator
  :map *authenticator-classes*
  :find find-authenticator-class)

(defclass standard-authenticator (authenticator)
  ((name :initarg :name :reader name)
   (textual :initarg :textual :reader authenticator-textual-p))
  (:default-initargs :textual nil)
  (:documentation "Represents a standard authentication mechanism."))

(defclass generic-authenticator (standard-authenticator)
  ()
  (:documentation "Represents an authentication mechanism that is not
supported by the D-BUS system."))

(defmethod authenticator-challenge ((mechanism generic-authenticator) challenge)
  (declare (ignore challenge))
  (values :error))

(defun parse-authentication-response (line &key as-string)
  "Parse authentication response line and return two values:

  :REJECTED

    Current authentication exchanged failed; the second value is a
    list of authentication mechanisms.

  :OK

    Client has been authenticated; the second value is the server's
    UUID.

  :DATA

    Data are available; the second value is either an octet vector or
    a string, depending on the value of AS-STRING.

  :AGREE-UNIX-FD

    The server supports Unix file descriptor passing; the second value
    is NIL.

  :ERROR

    Bad command or arguments; the second value is NIL.

  :UNEXPECTED

    Unexpected command; the second value is the response line."
  (cond ((starts-with-subseq "REJECTED " line)
         (values :rejected (split-sequence #\Space line :start 9)))
        ((starts-with-subseq "OK " line)
         (values :ok (subseq line 3)))
        ((starts-with-subseq "DATA " line)
         (let ((data (hex-string-to-octet-vector (subseq line 5))))
           (values :data (if as-string (sb-ext:octets-to-string data :external-format :utf-8) data))))
        ((equal "AGREE_UNIX_FD" line)
         (values :agree-unix-fd nil))
        ((starts-with-subseq "ERROR " line)
         (values :error nil))
        (t (values :unexpected line))))

(defun format-authentication-command (command &rest arguments)
  "Format and return authentication command line.  Command is one
of :AUTH, :CANCEL, :BEGIN, :DATA, :NEGOTIATE-UNIX-FD, or :ERROR, and
takes arguments in accordance with the D-BUS specification."
  (ecase command
    (:auth
     (destructuring-bind (&optional mechanism initial-response) arguments
       (format nil "AUTH ~@[~A~]~@[ ~A~]" mechanism initial-response)))
    (:cancel "CANCEL ")
    (:begin "BEGIN ")
    (:data
     (destructuring-bind (data) arguments
       (format nil "DATA ~A" (octet-vector-to-hex-string data))))
    (:negotiate-unix-fd "NEGOTIATE_UNIX_FD ")
    (:error
     (destructuring-bind (&optional explanation) arguments
       (format nil "ERROR ~@[~A~]" explanation)))))

(defun receive-authentication-response (connection &key as-string expect)
  "Receive authentication response line from the server.  If EXPECT is
NIL, just return the response command and argument.  Otherwise,
compare its value to the response command.  If they are the same, just
return the argument; otherwise, signal an authentication error."
  (multiple-value-bind (command argument)
      (parse-authentication-response (print (receive-line connection))
                                     :as-string as-string)
    (cond ((null expect) (values command argument))
          ((eq command expect) argument)
          (t (error 'dbus-auth-error :command command :argument argument)))))

(defun send-authentication-command (connection command &rest arguments)
  "Send an authentication command to the server."
  (send-line (apply #'format-authentication-command command arguments)
             connection))

;;; Introspection
;; TODO 2026-03-23: 
(defclass object ()
  ((connection :initarg :connection :reader connection)
   (path :initarg :path :reader path)
   (destination :initarg :destination :reader object-destination)
   (interfaces :initform (make-hash-table :test 'equal) :reader object-interfaces)))

(defmethod print-object ((object object) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S" (path object)))
  object)

(defun object-interface (name object)
  (gethash name (object-interfaces object)))

(defun (setf object-interface) (interface name object)
  (setf (gethash name (object-interfaces object)) interface))

(defun list-object-interfaces (object)
  (hash-table-values (object-interfaces object)))

(defun make-object (connection path destination interfaces)
  (let ((object (make-instance 'object :connection connection :path path :destination destination)))
    (dolist (interface interfaces)
      (setf (object-interface (name interface) object) interface))
    object))

(defclass dbus-interface ()
  ((name :initarg :name :reader name)
   (methods :initform (make-hash-table :test 'equal) :reader interface-methods)
   (properties :initform (make-hash-table :test 'equal) :reader interface-properties)
   (signals :initform (make-hash-table :test 'equal) :reader interface-signals)))

(defmethod print-object ((interface dbus-interface) stream)
  (print-unreadable-object (interface stream :type t)
    (format stream "~S" (name interface)))
  interface)

(defun interface-method (name interface)
  (gethash name (interface-methods interface)))

(defun interface-property (name interface)
  (gethash name (interface-properties interface)))

(defun interface-signal (name interface)
  (gethash name (interface-signals interface)))

(defun (setf interface-method) (method name interface)
  (setf (gethash name (interface-methods interface)) method))

(defun (setf interface-property) (property name interface)
  (setf (gethash name (interface-properties interface)) property))

(defun (setf interface-signal) (signal name interface)
  (setf (gethash name (interface-signals interface)) signal))

(defun list-interface-methods (interface)
  (hash-table-values (interface-methods interface)))

(defun list-interface-properties (interface)
  (hash-table-values (interface-properties interface)))

(defun list-interface-signals (interface)
  (hash-table-values (interface-signals interface)))

(defun make-dbus-interface (name methods properties signals)
  (let ((interface (make-instance 'dbus-interface :name name)))
    (dolist (method methods)
      (setf (interface-method (name method) interface) method))
    (dolist (property properties)
      (setf (interface-property (name property) interface) property))
    (dolist (signal signals)
      (setf (interface-signal (name signal) interface) signal))
    interface))

(defclass dbus-method ()
  ((name        :initarg :name      :reader name)
   (signature   :initarg :signature :reader method-signature)
   (arg-names   :initarg :args      :reader method-argument-names)
   (arg-types   :initarg :arg-types :reader method-argument-types)
   (results     :initarg :res       :reader method-result-types)))

(defmethod print-object ((method dbus-method) stream)
  (print-unreadable-object (method stream :type t)
    (format stream "~S ~A" (name method) (method-signature method)))
  method)

(defun make-dbus-method (name signature parm-names parm-types results)
  (make-instance 'dbus-method
                 :name name
                 :signature signature
                 :args      parm-names
                 :arg-types parm-types
                 :res results))

(defclass dbus-property ()
  ((name        :initarg :name   :reader name)
   (type        :initarg :type   :reader property-type)
   (access      :initarg :access :reader property-access)))

(defmethod print-object ((property dbus-property) stream)
  (print-unreadable-object (property stream :type t)
    (format stream "~S" (name property)))
  property)

(defun make-dbus-property (name type access)
  (make-instance 'dbus-property
                 :name name
                 :type type
                 :access access))

(defclass dbus-signal ()
  ((name        :initarg :name      :reader name)
   (arg-names   :initarg :args      :reader signal-argument-names)
   (arg-types   :initarg :arg-types :reader signal-argument-types)))

(defmethod print-object ((self dbus-signal) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~S" (name self)))
  self)

(defun make-dbus-signal (name parm-names parm-types)
  (make-instance 'dbus-signal
                 :name      name
                 :args      parm-names
                 :arg-types parm-types))

(defun dont-resolve-entities (a b)
  (declare (ignore a b))
  (make-in-memory-input-stream nil))

(defmacro defaulted-attribute (name default-value &body forms)
  `(let ((_ (or (optional-attribute ,name _) ,default-value)))
     ,@forms))

(defun parse-introspection-document (input)
  (xml-parse input)
  #+nil
  (with-xspam-source (make-xspam-source input :entity-resolver #'dont-resolve-entities)
    (element :node
      (let (interfaces)
        (one-or-more
         (element :interface
           (let (interface-name)
             (attribute :name (setf interface-name _))
             (let (methods properties signals)
               (zero-or-more
                (one-of
                 (element :method
                   (let (method-name
                         (signature (make-string-output-stream))
                         (parm-names ())
                         (parm-types ())
                         (result-types ()))
                     (attribute :name (setf method-name _))
                     (zero-or-more
                      ;; TODO: annotation
                      (element :arg
                        (defaulted-attribute :direction "in"
                          (when (equal _ "out")
                            (attribute :type
                              (push _ result-types)))
                          (when (equal _ "in")
                            (defaulted-attribute :name nil
                              (push _ parm-names))
                            (attribute :type
                              (push _ parm-types)
                              (write-string _ signature))))))
                     (push (make-dbus-method method-name
                                        (get-output-stream-string signature)
                                        (reverse parm-names)
                                        (reverse parm-types)
                                        (reverse result-types))
                           methods)))
                 (element :property
                   (let (property-name property-type property-access)
                     (attribute :name (setf property-name _))
                     (attribute :type (setf property-type _))
                     (attribute :access (setf property-access _))
                     (push (make-dbus-property property-name property-type property-access) properties)))
                 (element :signal
                   (let ((signal-name)
                         (parm-names ())
                         (parm-types ()))
                     (attribute :name (setf signal-name _))
                     (zero-or-more
                      (element :arg
                        (defaulted-attribute :name nil
                          (push _ parm-names))
                        (attribute :type (push _ parm-types))))
                     (push (make-dbus-signal signal-name
                                        (nreverse parm-names)
                                        (nreverse parm-types))
                           signals)))))
               (push (make-dbus-interface interface-name (nreverse methods) (nreverse properties) (nreverse signals)) interfaces)))))
        (nreverse interfaces)))))

(defun make-object-from-introspection (connection path destination)
  (make-object connection path destination
               (parse-introspection-document
                (fetch-introspection-document connection path destination))))

(defun fetch-introspection-document (connection path destination)
  (invoke-method connection "Introspect"
                 :path path
                 :destination destination
                 :interface "org.freedesktop.DBus.Introspectable"))

(defun signature-for-method (method-name interface-name object)
  (method-signature (interface-method method-name (object-interface interface-name object))))

(defun object-invoke (object interface-name method-name &rest args)
  (invoke-method (connection object)
                 method-name
                 :path (path object)
                 :interface interface-name
                 :destination (object-destination object)
                 :signature (signature-for-method method-name interface-name object)
                 :arguments args))

;;; Publish
(defgeneric publish-objects (connection &optional object-names))
(defgeneric dispatch-message (message object connection))
(defgeneric lookup-handler (message object))
(defgeneric apply-handler (handler message connection))
(defgeneric missing-handler (message connection))
(defgeneric signature-mismatch (expected-signature message connection))
(defgeneric handler-error (condition handler message connection))
(defgeneric method-handler-bad-results (results method message connection))
(defgeneric method-return-reply (results method message connection))
(defgeneric method-error-reply (error-name error-description message connection))

(defmethod publish-objects ((connection standard-dbus-connection) &optional (object-names *all-dbus-objects*))
  (let ((objects-by-path (make-object-index object-names)))
    ;; At this point we have an index by object path.  Note that if we
    ;; redefine an object with a new path later on, the index will be
    ;; stale.  Avoid doing that :)
    (loop
     (dolist (message (drain-pending-messages connection))
       (let ((object (gethash (path message) objects-by-path)))
         (if (null object)
             (missing-handler message connection)
             (dispatch-message message object connection))))
     (io/mux:event-dispatch (connection-event-base connection) :oneshot t))))

(defun make-object-index (object-names)
  (let ((index (make-hash-table :test 'equal)))
    (dolist (object-name object-names)
      (with-simple-restart (skip "Skip publishing object")
        (let ((object (require-dbus-object object-name)))
          (symbol-macrolet ((index-entry (gethash (path object) index)))
            (when (or (null index-entry)
                      (replace-entry-p index-entry object :error))
              (setf index-entry object))))))
    index))

(defun matching-signatures-p (signature1 signature2)
  (equal (signature signature1) (signature signature2)))

(defmethod dispatch-message (message (object dbus-object) (connection connection))
  (let ((handler (lookup-handler message object)))
    (cond ((null handler)
           (missing-handler message connection))
          ((not (matching-signatures-p (message-signature message)
                                       (handler-input-signature handler)))
           (signature-mismatch (handler-input-signature handler) message connection))
          (t
           (apply-handler handler message connection)))))

(defmethod lookup-handler ((message message) (object dbus-object))
  (gethash (full-member-name (message-interface message) (message-member message))
           (dbus-object-handler-lookup-table message object)))

(defmethod apply-handler ((handler dbus-signal-handler) (message dbus-signal-message) (connection connection))
  (handler-case
      (apply (handler-function handler) (message-body message))
    (error (condition)
      (handler-error condition handler message connection))))

(defmethod apply-handler ((handler dbus-method-handler) (message dbus-method-call-message) (connection connection))
  (let ((results (handler-case
                     (multiple-value-list
                      (apply (handler-function handler) (message-body message)))
                   (error (condition)
                     (handler-error condition handler message connection)
                     (return-from apply-handler)))))
    (if (valid-body-p results (handler-output-signature handler))
        (method-return-reply results handler message connection)
        (method-handler-bad-results results handler message connection))))

(defmethod missing-handler (message connection)
  (method-error-reply "MissingHandler"
                      (format nil "Missing ~A handler at path ~A interface ~A name ~A"
                              (if (typep message 'dbus-signal-message) "signal" "method-call")
                              (path message)
                              (message-interface message)
                              (message-member message))
                      message connection))

(defmethod signature-mismatch (expected-signature message connection)
  (method-error-reply "SignatureMismatch"
                      (format nil "Mismatching signature; expected=~S, actual=~S."
                              (signature expected-signature)
                              (signature (message-signature message)))
                      message connection))

(defmethod handler-error (condition (handler dbus-signal-handler) (message message) (connection connection))
  (warn "Signal handler signaled an error: ~A." condition))

(defmethod handler-error (condition (handler dbus-method-handler) (message message) (connection connection))
  (warn "Method handler ~S signaled an error: ~A."
        (handler-full-lisp-name handler) condition)
  (method-error-reply "MethodError"
                      (format nil "Method ~A signaled an error: ~A"
                              (name handler) condition)
                      message connection))

(defmethod method-handler-bad-results (results handler message connection)
  (cerror "Continue, sending an error to the bus"
          "Method handler ~S returned bad results; expected-signature=~S, results=~S."
          (handler-full-lisp-name handler)
          (handler-output-signature handler)
          results)
  (method-error-reply "InternalMethodError"
                      (format nil "Method ~A is buggy." (name handler))
                      message connection))

(defmethod method-return-reply (results handler message connection)
  (unless (logtest +message-no-reply-expected+ (message-flags message))
    (send-message
     (encode-dbus-message (message-endianness message) :method-return 0 1
                     (next-id connection) nil nil nil nil
                     (message-serial message) (message-sender message)
                     nil (handler-output-signature handler) results)
     connection)))

;; Do nothing; signals don't reply.
(defmethod method-error-reply (error-name error-description (message dbus-signal-message) connection)
  (declare (ignore error-name error-description connection)))

(defmethod method-error-reply (error-name error-description (message dbus-method-call-message) connection)
  (unless (logtest +message-no-reply-expected+ (message-flags message))
    (send-message
     (encode-dbus-message (message-endianness message) :error 0 1
                     (next-id connection) nil nil nil
                     ;; TODO: Not invent error names like that.
                     (concatenate 'string (message-interface message) ".Error." error-name)
                     (message-serial message) (message-sender message) nil
                     "s" (list error-description))
     connection)))

;;; DBUS
(defclass dbus (bus)
  ((connection :reader connection :initarg :connection)
   (name :initarg :name :reader name)))

(defun call-with-open-bus (function event-base server-addresses)
  (with-open-connection (connection event-base server-addresses)
    (authenticate (supported-authenticators connection) connection)
    (funcall function (make-instance 'dbus :name (hello connection) :connection connection))))

(defmacro with-open-bus ((bus-var server-addresses &key event-base) &body forms)
  (if (null event-base)
      (with-gensyms (event-base)
        `(with-event-base (,event-base)
           (with-open-bus (,bus-var ,server-addresses :event-base ,event-base)
             ,@forms)))
      (once-only (server-addresses event-base)
        `(call-with-open-bus (lambda (,bus-var) ,@forms) ,event-base ,server-addresses))))

(defmacro with-introspected-object ((name bus path destination) &body forms)
  (with-gensyms (object)
    `(let ((,object (make-object-from-introspection (connection ,bus) ,path ,destination)))
       (flet ((,name (interface-name method-name &rest args)
                (apply #'object-invoke ,object interface-name method-name args)))
         ,@forms))))

(defmethod publish-objects ((bus bus) &optional (object-names *all-dbus-objects*))
  (publish-objects (connection bus) object-names))

(defun hello (connection)
  (invoke-method connection "Hello"
                 :path "/org/freedesktop/DBus"
                 :interface "org.freedesktop.DBus"
                 :destination "org.freedesktop.DBus"))

(defun get-machine-id (bus)
  "Gets the Machine UUID of the machine hosting the object."
  (invoke-method (connection bus)
                 "GetMachineId"
                 :interface "org.freedesktop.DBus.Peer"
                 :path "/"))

(defun get-property (bus service object interface property)
  "Invokes the Get method to retrieve an object property."
  (invoke-method (connection bus)
                 "Get"
                 :destination service
                 :path object
                 :interface "org.freedesktop.DBus.Properties"
                 :signature "ss"
                 :arguments (list interface property)))

(defun get-all-properties (bus service object interface)
  "Invokes the GetAll method to retrieve all the properties of an object."
  (invoke-method (connection bus)
                 "GetAll"
                 :destination service
                 :path object
                 :interface "org.freedesktop.DBus.Properties"
                 :signature "s"
                 :arguments (list interface)))

(defun get-managed-objects (bus service object)
  (invoke-method (connection bus)
                 "GetManagedObjects"
                 :destination service
                 :path object
                 :interface "org.freedesktop.DBus.ObjectManager"
                 :signature ""))

(defun add-match (bus &rest parameters)
  "Invokes AddMatch bus method.  Valid parameters are:

  :type           (:signal, :method-call, :method-return, :error)
  :sender         bus-name
  :interface      interface-name
  :member         (method-name, symbol-name)
  :path           object-path
  :path-namespace object-path
  :destination    unique-name
  :argN [N=0~63]  string"
  (when (oddp (length parameters))
    (error "Even number of parameters needed.~%"))
  (flet ((unlispify-symbols (list)
           (loop for item in list
                 collecting (if (symbolp item)
                                (substitute #\_ #\- (format nil "~(~A~)" item))
                                (format nil "~A" item)))))
    (invoke-method
     (connection bus)
     "AddMatch"
     :destination "org.freedesktop.DBus"
     :path "/org/freedesktop/DBus"
     :interface "org.freedesktop.DBus"
     :signature "s"
     :arguments
     (list (format nil "~{~(~A~)=~A~^,~}" (unlispify-symbols parameters))))))

(defun request-name (bus name &rest flags)
  "Asks DBus to assign a name to the bus.  Valid flags
are :allow-replacement, :replace-existing, and :do-not-queue."
  (let ((flags-value
         (reduce #'logior
                 (mapcar (lambda (flag)
                           (case flag
                             (:allow-replacement 1)
                             (:replace-existing  2)
                             (:do-not-queue      4)
                             (t (error "Invalid flag ~A.~%" flag))))
                         flags))))
    (case (invoke-method (connection bus)
                         "RequestName"
                         :destination "org.freedesktop.DBus"
                         :path "/org/freedesktop/DBus"
                         :interface "org.freedesktop.DBus"
                         :signature "su"
                         :arguments (list name flags-value))
      (1 :primary-owner)
      (2 :in-queue)
      (3 :exists)
      (4 :already-owner)
      (t (error "Unknown response received.~%")))))

(defun list-names (bus)
  "Returns a list of all currently-owned names on the bus via
ListNames method invocation."
  (invoke-method (connection bus)
                 "ListNames"
                 :destination "org.freedesktop.DBus"
                 :path "/"
                 :interface "org.freedesktop.DBus"
                 :signature ""))
