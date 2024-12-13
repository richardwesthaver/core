;;; net/srv.lisp --- Lisp Web Services

;; This library contains provides a Web Server abstraction a la Hunchentoot or
;; Woo.

;;; Commentary:

;; The code in this file is meant to be small. We want to leverage the core
;; ecosystem and internal NET/* packages to build high-level abstractions that
;; are still useful with minimal boilerplate.

;; In other words we want to support both these use-cases in the least amount
;; of code:
#|
(srv:start (srv:file-server)) ;; start a simple HTTP file server in current
                              ;; directory with all default values

(srv:defservice my-homepage (:port 8080
                                 :auth (auth settings ...)
                                 :routes (routes ...)
                                 &rest ...)
 (with-service (ws 'my-homepage)
  (srv:start ws)))
|#

;; not considering SSL currently - not a core object type but perhaps subclass

;; mostly following the implementation of hunchentoot with attempts at
;; simplification.

;;; Code:
(in-package :net/srv)

(pkg:defpkg :net/srv/http
  (:use :cl :std :net/proto/http
   :net/codec/http :net/core :net/cookie :io/chunky)
  (:use-reexport :net/srv)
  (:export :http-service :https-service))

(pkg:defpkg :net/srv/udp
  (:use :cl :std :net/udp :net/codec/tlv :net/core)
  (:use-reexport :net/srv)
  (:export :udp-service :echo-service))

(pkg:defpkg :net/srv/oauth
  (:use :cl :std :net/codec/http :net/core :net/cookie :net/core :id :secret :uri :net/srv/http)
  (:import-from :cli/tools/web :browse-url)
  (:use-reexport :net/srv)
  (:export :udp-service :echo-service))

;;; Vars
(defvar *router*)
(defvar *service*)
(defvar *service-table* (make-hash-table :weakness :value))
(defvar-unbound *request*)
(defvar-unbound *response*)
(defvar-unbound *session*)
(defvar-unbound *session-secret*)
(defvar-unbound *service-stream*)
(defvar-unbound *finish-processing-socket*)
(defvar-unbound *close-service-stream*)
(defvar *headers-sent* nil
  "Used internally to check whether the response headers have
already been sent for this request.")
(defvar *service-header-stream* nil)
(defun in-request-p () (and (boundp '*request*) *request*))
(defun in-response-p () (and (boundp '*response*) *response*))
(defvar *session-db* nil)
(defvar *global-session-db-lock* (load-time-value (make-mutex :name "global-session-db")))
(defvar *log-service-errors* t)

(defvar *access-log-lock* (make-mutex :name "access-log"))
(defvar *message-log-lock* (make-mutex :name "message-log"))
(defvar *default-connection-timeout* 20)
(defvar *default-connection-max* 16)
(defvar *default-service-port* 8080)
(defvar *default-max-thread-count* 100)
(defvar *default-max-accept-count* (+ *default-max-thread-count* 20))
(defvar *default-ssl-service-port* 8443)
(defvar *default-session-timeout* #.(* 30 60)) ;; 30m
;;; Conditions
;; from hunchentoot
(define-condition service-condition (condition) ())
(eval-always
  (deferror service-error (service-condition error) () (:auto t)))
(deferror simple-service-error (service-error simple-condition) () (:auto t))

(define-condition service-warning (service-condition warning) ())

(defwarning simple-service-warning (service-warning simple-warning) () (:auto t))

(deferror bad-request (service-error) ())

;;; Utils
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun default-web-directory (&optional sub-directory)
    (let ((source-directory #.(or *compile-file-truename* *load-truename*)))
      (merge-pathnames (make-pathname :directory (append (pathname-directory source-directory)
                                                         (list "www")
                                                         (when sub-directory
                                                           (list sub-directory)))
                                      :name nil
                                      :type nil
                                      :defaults source-directory)))))

;; Global Helpers
;;; Protocol
(defgeneric service (self)
  (:method ((self t)) (when (boundp '*service*) *service*))
  (:method ((self symbol)) (gethash self *service-table*))
  (:method ((self string)) (gethash (symbolicate (string-upcase self)) *service-table*)))

(defgeneric start-listening (self))
(defgeneric service-status-message (service status-code &key &allow-other-keys))
(defgeneric restart-service (self)
  (:documentation "Restart a service.")
  (:method ((self t))
    (stop self)
    (start self)))
(defgeneric execute-service (self)
  (:documentation "A function that is called by a service once it has been initialized. Usually calls the 'accept-connections' method of the service."))
(defgeneric find-route (self uri))
(defgeneric add-route (self uri srv &key &allow-other-keys))
(defgeneric delete-route (self uri &key &allow-other-keys))
(defgeneric handle-request (self request)
  (:documentation "Function called after fetching a request. Used to establish error handling,
logging, etc."))
(defgeneric dispatch-request (self request)
  (:documentation "Function called after 'handle-request' which routes a request to a service."))
(defgeneric send-response (service stream &key content &allow-other-keys))
(defgeneric accept-connections (self))
(defgeneric handle-connection (self conn))
(defgeneric initialize-connection-hook (self stream))
(defgeneric reset-connection-stream (self stream))
(defgeneric process-request (req)
  (:documentation "Function called by PROCESS-CONNECTION after reading incoming headers. Calls
HANDLE-REQUEST to dispatch to a route and return output to the client using
START-OUTPUT.

Return value is ignored."))
(defgeneric process-connection (self socket))
(defgeneric secure-service-p (self)
  (:method ((self t)) 
    (declare (ignore self))
    nil))

(defgeneric service-log-message (self level format-string &rest arguments))
(defgeneric service-log-access (self &optional code))

(defgeneric response-ok-p (res))
(defgeneric response-status (res))
(defgeneric (setf response-status) (new res))

;;; Response
(defclass response () ())
(defmethod response-ok-p (res) t)

;;; Request
(defclass request ()
  ((service :initarg :service
           :reader service)
   (session :initform nil
            :accessor session)
   (protocol :initarg :request-protocol :reader request-protocol)
   (local-addr :initarg :local-addr :reader local-addr)
   (local-port :initarg :local-port :reader local-port)
   (remote-addr :initarg :remote-addr :reader remote-addr)
   (remote-port :initarg :remote-prot :reader remote-port)
   (content-stream :initarg :content-stream :reader content-stream)
   (data :initarg :data :accessor request-data)))

(defun remote-addr* (&optional (request *request*))
  "Returns the address the current request originated from."
  (remote-addr request))

(defun remote-port* (&optional (request *request*))
  "Returns the port the current request originated from."
  (remote-port request))

(defun local-addr* (&optional (request *request*))
  "Returns the address the current request connected to."
  (local-addr request))

(defun local-port* (&optional (request *request*))
  "Returns the port the current request connected to."
  (local-port request))

(defun request-protocol* (&optional (request *request*))
  "Returns the request protocol as a Lisp keyword."
  (request-protocol request))

;;; Session
(defmacro with-session-db-lock (lock &body body)
  (with-gensyms (th)
    (once-only (lock)
      `(flet ((,th () ,@body))
         (cond (,lock (with-mutex (,lock) (,th)))
               (t (,th)))))))

(defgeneric remove-session-hook (service session))

(defgeneric session-db (self)
  (:method ((self t))
    *session-db*)
  (:method ((self db:database))
    (db:db self)))

(defgeneric (setf session-db) (new self)
  (:method (new (self db:database))
    (setf (db:db self) new)))

(defgeneric next-session-id (service))

(let ((session-id-counter 0))
  (defmethod next-session-id ((service t))
    (incf session-id-counter)))

;; HACK 2024-07-18: currently not storing the SESSION-STRING directly in this
;; class as a slot. may need to change but I would rather have the string
;; cached/displaced to some other location.. depends how often we need that
;; string.

;; session-cookie-value, session-verify

(defclass session (id:id)
  ((start :initarg :start :initform (get-universal-time) :accessor start)
   (data :initarg :data :accessor data)
   (timeout :type fixnum :accessor timeout :initarg :timeout))
  (:default-initargs
   :start (get-universal-time)
   :timeout *default-session-timeout*))
   
(defclass session-database (database) 
  ((lock :initarg :lock :initform (make-mutex :name "session-db") :accessor lock)))

(defun remove-session (session)
  "Remove SESSION from the global session database."
  (with-session-db-lock (lock *service*)
    (remove-session-hook *service* session)
    (setf (session-db *service*)
          (delete (id:id session) (session-db *service*)
                  :key #'car :test #'=))))

(defun reset-session-secret ()
  (setq *session-secret* (random-chars 32)))

(defun session-value (sym &optional (session *session*))
  (when session
    (let ((found (assoc sym (data session) :test #'eq)))
      (values (cdr found) found))))

(defsetf session-value (sym &optional session) (new-val)
  (once-only (sym)
    (with-gensyms (place %session)
      `(let ((,%session (or ,session (start-session))))
         (with-session-db-lock (lock *service*)
           (let* ((,place (assoc ,sym (data ,%session) :test #'eq)))
             (cond
               (,place
                (setf (cdr ,place) ,new-val))
               (t
                (push (cons ,sym ,new-val)
                      (slot-value ,%session 'data))
                ,new-val))))))))

(defun delete-session-value (sym &optional (session *session*))
  (when session
    (setf (slot-value session 'data)
          (delete sym (data session)
                  :key 'car :test 'eq))))

(defgeneric session-created (service new-session))

(defvar *session-gc-frequency* 60)

(defgeneric session-expired-p (self)
  (:method ((self session))
    (< (+ (start self) (timeout self))
       (get-universal-time))))

(defun session-gc ()
  "Removes sessions from the global session database which have expired or are invalid."
  (with-session-db-lock (lock *service*)
    (setf (session-db *service*)
          (loop for pair in (session-db *service*)
                for (nil . session) = pair
                when (session-expired-p session)
                do (remove-session-hook *service* session)
                else
                collect pair)))
  (values))

(let ((session-usage-counter 0))
  (defmethod session-created ((service t) (session t))
    (when (and *session-gc-frequency*
               (zerop (mod (incf session-usage-counter)
                           *session-gc-frequency*)))
      (session-gc))))

(defun start-session (&optional (session-class 'session))
  (let ((session (session *request*)))
    (when session
      (return-from start-session session))
    (with-session-db-lock (lock *service*)
      (setf session (make-instance session-class))
      (setf (session *request*) session
            (session-db *service*)
            (acons (id:id session) session (session-db *service*))))
    (session-created *service* session)
    (setq *session* session)))

(defgeneric session-verify (request))

(defun reset-sessions (&optional (service *service*))
  (with-session-db-lock (lock service)
    (loop for (nil . s) in (session-db service)
          do (remove-session-hook service s))
    (setq *session-db* nil))
  (values))
          
;;; Headers

;;; Logger
(defclass service-logger (logger) 
  ((access-log-output :accessor access-log-output :initarg :access-log-output)
   (message-log-output :accessor message-log-output :initarg :message-log-output))
  (:default-initargs
   :access-log-output *error-output*
   :message-log-output *error-output*))

;;; Engine
;; Multithreaded runtime for services

(define-task-kernel service-task-kernel () ()
  "Default task kernel for service-based tasks.")

(defclass engine () ((service :accessor service)))

(defclass single-threaded-engine (engine) ())

(defclass multi-threaded-engine (engine)
  ((process :accessor process)))

(defmethod execute-service ((self multi-threaded-engine))
  (setf (process self)
        (run-thread 
         self
         (lambda () (accept-connections (service self)))
         :name (format nil "service-~A:~A"
                       (or (address (service self)) "*")
                       (port (service self))))))
                    
;; Note from Hunchentoot:
#|
;; You might think it would be nice to provide a taskmaster that takes
;; threads out of a thread pool.  There are two things to consider:
;;  - On a 2010-ish Linux box, thread creation takes less than 250 microseconds.
;;  - Bordeaux Threads doesn't provide a way to "reset" and restart a thread,
;;    and it's not clear how many Lisp implementations can do this.
;; If you're still interested, use the quux-hunchentoot extension to hunchentoot.
|#
(defclass thread-per-connection-engine (multi-threaded-engine)
  ((max-thread-count
    :type (or integer null)
    :initarg :max-thread-count
    :initform nil
    :accessor max-thread-count)
   (thread-count
    :type integer
    :initform 0
    :accessor thread-count)
   (thread-count-lock
    :initform (make-mutex :name "thread-count")
    :accessor thread-count-lock)
   (max-accept-count
    :type (or integer null)
    :initarg :max-accept-count
    :initform nil
    :accessor max-accept-count)
   (accept-count
    :type integer
    :initform 0
    :accessor accept-count)
   (accept-count-lock
    :initform (make-mutex :name "accept-count")
    :reader accept-count-lock)
   (wait-queue
    :initform (sb-concurrency:make-queue)
    :reader wait-queue)
   (wait-lock
    :initform (make-mutex :name "wait-queue")
    :reader wait-lock)
   (worker-thread-name-format
    :type (or string null)
    :initarg :worker-thread-name-format
    :initform "service-worker-~A"
    :accessor worker-thread-name-format))
  (:default-initargs
   :max-thread-count *default-max-thread-count*
   :max-accept-count *default-max-accept-count*))

(defmethod initialize-instance :after ((self thread-per-connection-engine) &rest args)
  "Ensure MAX-ACCEPT-COUNT > MAX-THREAD-COUNT."
  (declare (ignore args))
  (when (max-accept-count self)
    (unless (max-thread-count self)
      (error "MAX-THREAD-COUNT must be supplied if MAX-ACCEPT-COUNT is."))
    (unless (> (max-accept-count self) (max-thread-count self))
      (error "MAX-ACCEPT-COUNT must be greater than MAX-THREAD-COUNT"))))

(defmethod increment-accept-count ((self thread-per-connection-engine))
  (when (max-accept-count self)
    (with-mutex ((accept-count-lock self))
      (incf (accept-count self)))))

(defmethod decrement-accept-count ((self thread-per-connection-engine))
  (when (max-accept-count self)
    (with-mutex ((accept-count-lock self))
      (decf (accept-count self)))))

(defmethod increment-thread-count ((self thread-per-connection-engine))
  (when (max-thread-count self)
    (with-mutex ((thread-count-lock self))
      (incf (thread-count self)))))

(defmethod decrement-thread-count ((self thread-per-connection-engine))
  (when (max-thread-count self)
    (with-mutex ((thread-count-lock self))
      (decf (thread-count self)))))

(defmethod wait-for-free-connection ((self thread-per-connection-engine))
  (with-mutex ((wait-queue self))
    (loop until (< (thread-count self) (max-thread-count self))
          do (sb-thread:condition-wait (wait-lock self) (wait-queue self)))))

(defmethod run-thread ((self thread-per-connection-engine) thunk &key name)
  (sb-thread:make-thread thunk :name name))

(defmethod %handle-connection ((self thread-per-connection-engine) socket)
  (increment-accept-count self)
  (flet ((pconn (service socket)
           (increment-thread-count self)
           (unwind-protect (process-connection service socket)
             (decrement-thread-count self))))
    (cond ((null (max-thread-count self))
           (process-connection (service self) socket))
          ((if (max-accept-count self)
               (>= (accept-count self) (max-accept-count self))
               (>= (thread-count self) (max-thread-count self)))
           (too-many-engine-requests self socket)
           (send-service-unavailable-response self socket))
          ((and (max-accept-count self)
                (>= (thread-count self) (max-thread-count self)))
           (wait-for-free-connection self)
           (pconn (service self) socket))
          (t
           (pconn (service self) socket)))))

(defmethod create-request-worker-thread ((self thread-per-connection-engine) socket)
  "Create a thread which handles a request."
  (handler-case
      (run-thread
       self
       (lambda () (%handle-connection self socket))
       :name (format nil (worker-thread-name-format self) (socket-peername socket)))
    (error (c)
      (let ((*service* (service self)))
        (ignore-errors
         (close (socket-make-stream (socket *service*)) :abort t))
        (log-message* :error
                      "Error while creating worker thread for new connection: ~A" c)))))

(defmethod stop ((self engine) &key)
  self)
(defmethod stop ((self thread-per-connection-engine) &key)
  (sb-thread:join-thread (process self)))

(defun too-many-engine-requests (self socket)
  (declare (ignore socket))
  (service-log-message (service self)
                       :warning "Unable to handle new request, too many active request threads"))

(defun send-service-unavailable-response (engine socket)
  (let* ((service (service engine))
         (*service* service)
         (*service-stream* (socket-make-stream socket)))
    (unwind-protect
         ;; handle conditions
         (let* ((*service-stream* (initialize-connection-hook service *service-stream*))
                (*request* (service-make-request service socket))
                (*response* (make-instance (service-response-class service))))
           (send-response service
                          *service-stream*
                          :content (service-status-message service 503)))
      (decrement-accept-count engine)
      (when *service-stream*
        (ignore-errors
         (finish-output *service-stream*))
        (ignore-errors
         (close *service-stream* :abort t))))))

(defmethod handle-connection ((self engine) socket)
  (create-request-worker-thread self socket))

;; supervisor, worker, task, kernel
;;; Service
(defclass service (id:id)
  ((port :reader port :initarg :port)
   (address :reader address :initarg :address)
   (request-class :type symbol :initarg :request-class :accessor service-request-class)
   (response-class :type symbol :initarg :response-class :accessor service-response-class)
   (engine :type service-engine :accessor engine :initarg :engine)
   ;; TODO 2024-12-08: hunchentoot uses read-timeout/write-timeout - figure out if needed
   (timeout :type fixnum :initarg :timeout :accessor timeout)
   (connection-max :type (or fixnum null) :initarg :connection-max)
   (logger :type service-logger :initarg :logger :reader logger)
   (socket :type (or null socket) :accessor socket :initarg :socket :initform nil)
   (backlog :accessor backlog :initarg :backlog
            :documentation "Number of pending connections allowed before the service will start bouncing.")
   (request-count :type integer :accessor request-count :initarg :request-count)
   (shutdown-p :type boolean :accessor shutdown-p :initarg :shutdown-p)
   (shutdown-lock :type mutex :accessor shutdown-lock :initarg :shutdown-lock)
   (shutdown-queue :type waitqueue :accessor shutdown-queue :initarg :shutdown-queue))
  (:default-initargs
   :id (symbol-name (gensym "service"))
   :port *default-service-port*
   :engine (make-instance 'thread-per-connection-engine)
   :address nil
   :request-class 'request
   :response-class 'response
   :timeout *default-connection-timeout*
   :connection-max *default-connection-max*
   :logger (make-instance 'service-logger)
   :backlog -1 ;; TODO 2024-10-23: what is a correct initial value here? wookie uses -1
   :request-count 0
   :shutdown-p t
   :shutdown-lock (sb-thread:make-mutex :name "shutdown-lock")
   :shutdown-queue (sb-thread:make-waitqueue :name "shutdown-queue"))
  (:documentation "The service class is designed primarily for webservers and functionally
similar to HUNCHENTOOT:ACCEPTOR."))

(defmethod name ((self service))
  (id:id self))

(defmethod message-log-output ((self service))
  (message-log-output (logger self)))

(defmethod access-log-output ((self service))
  (access-log-output (logger self)))

(defmethod print-object ((self service) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A on port ~A"
            (or (address self) "*") (port self))))

(defaccessor (sesion-db) ((self service)) *session-db*)

(defmethod service-log-message ((self service) level format-string &rest args)
  (log:with-log-stream (stream (message-log-output self) *message-log-lock*)
    (handler-case
        (format stream "[~A~@[ [~A]~]] ~?~%"
        (obj/time:iso-time) level
        format-string args)
      (error (e)
        (ignore-errors
         (format *trace-output* "error ~A while writing to error log, error not logged~%" e))))))

(defun log-message* (level format-string &rest args)
  (apply 'service-log-message *service* level format-string args))

(defmethod start-listening :around ((self service))
  (when (socket self)
    (simple-service-error "service ~A is already listening" self))
  ;; setup the socket and call SOCKET-LISTEN
  (call-next-method)
  (when (zerop (port self))
    (setf (slot-value self 'port) (nth-value 1 (socket-name (socket self))))))

(defmethod start-listening ((self service))
  (when (socket self)
    (simple-service-error "service ~A is already listening" self))
  (setf (socket self) (make-instance 'inet-socket :type :stream :protocol :tcp))
  (socket-bind (socket self)
               (or (address self)
                   #(0 0 0 0))
               (port self))
  (socket-listen (socket self)
                 (backlog self))
  (values))

(defmacro with-open-socket ((var socket) &body body)
  "Bind SOCKET to VAR and eval BODY followed by calling SOCKET-CLOSE on SOCKET."
  (once-only (socket)
    `(let ((,var ,socket))
       (unwind-protect (when ,var ,@body)
         (when ,var (socket-close ,var))))))
       
(defmethod accept-connections ((self service))
  (with-open-socket (sock (socket self))
    (loop
      (with-mutex ((shutdown-lock self))
        (when (shutdown-p self)
          (return))
        (when (socket-listen sock (backlog self))
          (when-let ((conn
                      (handler-case (socket-accept sock)
                        (sb-bsd-sockets::connection-refused-error ()))))
            (setf (sb-impl::fd-stream-timeout (socket-make-stream conn))
                  (coerce (timeout self) 'single-float))
            (handle-connection (engine self) conn)))))))

;; (defmethod dispatch-request ((self service) request))

;; (defmethod handle-request ((*service* service) (*request* request)))

;; (defmethod service-status-message )

(defmethod start ((self service))
  (setf (shutdown-p self) nil)
  (let ((engine (engine self)))
    (setf (service engine) self)
    (execute-service engine)))

(defmethod started-p ((self service))
  (and (socket self) t))

(defun wake-service-for-shutdown (service)
  "Create a dummy connection to the service, waking ACCEPT-CONNECTIONS while it is waiting. The idea is to force a check of SHUTDOWN-P."
  (handler-case
      (multiple-value-bind (address port) (sb-bsd-sockets:get-host-by-address (sb-bsd-sockets:socket-name (socket service)))
        (let ((conn (sb-bsd-sockets:socket-connect
                     (cond
                       ((and (= (length address) 4) (zerop (elt address 0)))
                        #(127 0 0 1))
                       ((and (= (length address) 16)
                             (every #'zerop address))
                        #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
                       (t address))
                     port)))
          (sb-bsd-sockets:socket-close conn)))
    (error (e)
      (service-log-message service :error "Wake-for-shutdown connect failed: ~A" e))))
                        
(defmethod stop ((self service) &key graceful)
  (with-mutex ((shutdown-lock self))
    (setf (shutdown-p self) t)
    (wake-service-for-shutdown self)
    (when graceful
      (with-mutex ((shutdown-lock self))
        (when (plusp (request-count self))
          (sb-thread:condition-wait (shutdown-queue self)
                                    (shutdown-lock self)))))
    (stop (engine self))
    (std:if-let ((sock (socket self)))
      (progn
        (sb-bsd-sockets:socket-close sock)
        (setf (socket self) nil))
      (net-warning "service socket unbound"))
    self))

(defmethod initialize-connection-hook ((self service) stream)
  stream)

(defmethod process-connection :around ((*service* service) (socket t))
  (with-logger *service*
    ;; (with-conditions-caught-and-logged ()
    ;; (with-mapped-conditions ()
    (call-next-method))) ;; )

(defun do-with-request-count-incf (*service* function)
  (with-mutex ((shutdown-lock *service*))
    (incf (request-count *service*)))
  (unwind-protect
       (funcall function)
    (with-mutex ((shutdown-lock *service*))
      (decf (request-count *service*))
      (when (shutdown-p *service*)
        (sb-thread:condition-broadcast (shutdown-queue *service*))))))

(defmacro with-request-count-incf (service &body body)
  "Execute BODY with REQUEST-COUNT of SERVICE
  incremented by one.  If the SHUTDOWN-P returns true after
  the BODY has been executed, the SHUTDOWN-QUEUE condition
  variable of the SERVICE is signalled in order to finish shutdown
  processing."
  `(do-with-request-count-incf ,service (lambda () ,@body)))

(defmethod remove-session-hook ((service service) (session t))
  nil)

(defmethod service-make-request (service socket &key 
                                         content-stream
                                         remote local
                                         protocol)
  "Make a REQUEST instance for SERVICE."
  (multiple-value-bind (raddr rport)
      (if remote
          (values-list remote)
          (sb-bsd-sockets:socket-peername socket))
    (multiple-value-bind (laddr lport)
        (if local
            (values-list local)
            (socket-name socket))
      (make-instance (service-request-class service)
        :service service
        :local-addr laddr
        :local-port lport
        :remote-addr raddr
        :remote-port rport
        :content-stream content-stream
        :protocol protocol))))

(defgeneric detach-socket (self)
  (:method ((self service))
    (setf *finish-processing-socket* t
          *close-service-stream* nil)))

(defun printable-ascii-char-p (char)
  (<= 32 (char-code char) 126))

(defun get-request-data (stream)
  "Reads incoming headers from the client via STREAM.  Returns as
multiple values the headers as an alist, the method, the URI, and the
protocol of the request."
  (with-character-stream-semantics
    (let ((first-line (read-initial-request-line stream)))
      (when first-line
        (unless (every #'printable-ascii-char-p first-line)
          (send-bad-request-response stream "Non-ASCII character in request line")
          (return-from get-request-data nil))
        (destructuring-bind (&optional method url-string protocol)
            (ppcre:split "\\s+" first-line :limit 3)
          (cond ;; ((not
                ;;   (setf method
                ;;         (find method +valid-request-methods+ :test #'string-equal)))
                ;;  (send-bad-request-response stream)
                ;; (return-from get-request-data nil))
                ((not url-string)
                 (send-bad-request-response stream)
                 (return-from get-request-data nil))
                ((not protocol)
                 ;; HTTP/1.1 specifies that if protocol is not provided
                 ;; then assume protocol version to be 1.0
                 (setf protocol :http/1.0))
                ;; ((not
                ;;   (setf protocol
                ;;         (find protocol +valid-protocol-versions+ :test #'string-equal)))
                ;;  (send-unknown-protocol-response stream)
                ;;  (return-from get-request-data nil))
                )
          (when *service-header-stream*
            (format *service-header-stream* "~A~%" first-line))
          (let ((headers (read-http-headers stream *service-header-stream*)))
            ;; maybe handle 'Expect: 100-continue' header
            (when-let ((expectations (cdr #|assoc*|# (assoc :expect headers))))
              (when (member "100-continue" (cl-ppcre:split "\\s*,\\s*" expectations) :test #'equalp)
                ;; according to 14.20 in the RFC - we should actually
                ;; check if we have to respond with 417 here
                (let ((continue-line
                        (format nil "HTTP/1.1 ~D ~A"
                                +http-continue+
                                (reason-phrase net/proto/http::+http-continue+))))
                  (write-sequence (map 'list #'char-code continue-line) stream)
                  (write-sequence +crlf+ stream)
                  (write-sequence +crlf+ stream)
                  (force-output stream)
                  (when *service-header-stream*
                    (format *service-header-stream* "~A~%" continue-line)))))
            (values headers method url-string protocol)))))))

(defmethod process-connection ((*service* service) (socket t))
  (let* ((socket-stream (sb-bsd-sockets:socket-make-stream socket))
         (*service-stream*)
         (*close-service-stream* t)
         (remote (multiple-value-list (socket-peername socket)))
         (local (multiple-value-list (socket-name socket))))
    (unwind-protect
         ;; process requests until shutdown signal is received or the peer
         ;; fails to send a request
         (progn
           (setq *service-stream* (initialize-connection-hook *service* socket-stream))
           (loop
             (let ((*finish-processing-socket* t))
               (when (shutdown-p *service*)
                 (return))
               (multiple-value-bind (headers-in method url-string protocol)
                   (get-request-data *service-stream*)
                 ;; check if there was a request at all
                 (unless method
                   (return))
                 (let ((*response* (make-instance (service-response-class *service*)))
                       (*session* nil)
                       (transfer-encodings (cdr #|assoc*?|# 
                                            (assoc :transfer-encoding headers-in))))
                   (when transfer-encodings
                     (setq transfer-encodings
                           (cl-ppcre:split "\\s*,\\s*" transfer-encodings))
                     (when (member "chunked" transfer-encodings :test #'equalp)
                       (setf *service-stream* (io/chunky:make-chunked-stream *service-stream*))))
                   (with-request-count-incf *service*
                     (process-request 
                      (service-make-request *service* socket
                                            :headers-in headers-in
                                            :content-stream *service-stream*
                                            :method method
                                            :uri url-string
                                            :remote remote
                                            :local local
                                            :server-protocol protocol))))
                 (finish-output *service-stream*)
                 (setq *service-stream* (reset-connection-stream *service* *service-stream*))
                  (when *finish-processing-socket*
                    (return))))))
      (when *close-service-stream*
        (flet ((close-stream (stream)
                 ;; as we are at the end of the request here, we ignore all
                 ;; errors that may occur while flushing and/or closing the
                 ;; stream.
                 (ignore-errors
                  (finish-output stream))
                 (ignore-errors
                  (close stream :abort t))))
          (unless (or (not *service-stream*)
                      (eql socket-stream *service-stream*))
            (close-stream *service-stream*))
          (close-stream socket-stream))))))

;;; Macros
(defmacro defservice (name &rest initargs)
  "Define a subclass of NET/SRV:SERVICE."
  `(defclass ,name ,@initargs))

(defmacro defroute (spec args &body body)
  "Define a new ROUTE with BODY and optionally register it with a URI. The
resulting function is stored within the *ROUTER* collection and may be
dispatched to by a SERVICE instance.

SPEC is either a symbol NAME or a list matching the
destructuring lambda list

  (name &key uri service-names host
        default-parameter-type default-request-type).

ARGS is a list the elements of which are either a symbol
VAR or a list matching the destructuring lambda list

  (var &key real-name parameter-type init-form request-type).")
