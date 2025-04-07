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
(start (srv/http:http-file-server)) ;; start a simple HTTP file server in current
                              ;; directory with all default values

(srv:defservice my-homepage (:port 8080
                                 :auth (auth settings ...)
                                 :routes (routes ...)
                                 &rest ...)
 (with-service (ws 'my-homepage)
  (srv:start ws)))
|#

;; mostly following the implementation of hunchentoot with attempts at
;; simplification.

;;; Code:
(in-package :net/srv)

(pkg:defpkg :net/srv/ext
  (:use :cl :std :net/core :cli/tools/net)
  (:export :caddy-service :nginx-service))

(pkg:defpkg :net/srv/http
  (:use :cl :std :net/proto/http
   :net/codec/http :net/core :net/cookie :io/chunky :srv)
  (:import-from :net/srv :service-log)
  (:use-reexport :net/srv)
  (:package-local-nicknames
   :codec :net/codec/http
   :proto :net/proto/http)
  (:export :http-service :https-service))

(pkg:defpkg :net/srv/udp
  (:use :cl :std :net/udp :net/codec/tlv :net/core :srv)
  (:use-reexport :net/srv)
  (:export :udp-service :echo-service))

(pkg:defpkg :net/srv/oauth
  (:use :cl :std :net/codec/http :net/core :net/cookie :net/core :id :secret :uri :net/srv/http :srv)
  (:import-from :cli/tools/net :browse-url)
  (:use-reexport :net/srv)
  (:export :udp-service :echo-service))

;;; Vars
(defvar *router*)
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
(defvar *default-session-timeout* #.(* 30 60)) ;; 30m
(defvar-unbound *session*)
(defvar-unbound *session-secret*)
(defvar-unbound *service-stream*)
(defvar-unbound *finish-processing-socket*)
(defvar-unbound *close-service-stream*)

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

(defgeneric make-service (self &rest args &key &allow-other-keys))
(defgeneric start-listening (self))
(defgeneric service-status-message (service status-code &key &allow-other-keys))
(defgeneric find-route (self uri))
(defgeneric add-route (self uri srv &key &allow-other-keys))
(defgeneric delete-route (self uri &key &allow-other-keys))
(defgeneric accept (self))
(defgeneric handle-connection (self conn))
(defgeneric initialize-connection-hook (self stream))
(defgeneric reset-connection-stream (self stream))
(defgeneric process-connection (self socket))
(defgeneric secure-service-p (self)
  (:method ((self t)) 
    (declare (ignore self))
    nil))

(defgeneric service-log-message (self level format-string &rest arguments))
(defgeneric service-log-access (self &optional code))

;;; Conditions
(defun abort-request-handler (&optional result)
  "This function can be called by a request handler at any time to
immediately abort handling the request.  This works as if the handler
had returned RESULT.  See the source code of REDIRECT for an example."
  (throw 'handler-done result))

;;; Classes
(defclass net-response (response) ())

 (defclass net-request (request)
  ((local-addr :initarg :local-addr :reader local-addr)
   (local-port :initarg :local-port :reader local-port)
   (remote-addr :initarg :remote-addr :reader remote-addr)
   (remote-port :initarg :remote-prot :reader remote-port)))

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
  (uri-scheme (uri request)))

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
   (timeout :type fixnum :accessor session-timeout :initarg :timeout))
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
    (< (+ (start self) (session-timeout self))
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
          
;;; Logger
(defclass service-logger (logger) 
  ((access-log-output :accessor access-log-output :initarg :access-log-output)
   (message-log-output :accessor message-log-output :initarg :message-log-output))
  (:default-initargs
   :access-log-output *error-output*
   :message-log-output *error-output*))

;;; Engine
(defclass single-threaded-engine (engine) ())

;; Multithreaded runtime for services
(defclass multi-threaded-engine (engine)
  ((process :accessor process)))

(defmethod run-thread ((self multi-threaded-engine) thunk &key name)
  (sb-thread:make-thread thunk :name name))

(defmethod exec ((self multi-threaded-engine))
  (setf (process self)
        (run-thread 
         self
         (lambda () (accept (service self)))
         :name (format nil "~A ~A ~A"
                       (name (service self))
                       (or (address (service self)) "*")
                       (port (service self)))))
  (values))
                    
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
    :initform "srv-worker-~A"
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

(defmethod exec ((self thread-per-connection-engine))
  (setf (process self)
        (run-thread 
         self
         (lambda () (accept (service self)))
         :name (format nil "~A ~A ~A"
                       (name (service self))
                       (or (address (service self)) "*")
                       (port (service self)))))
  (values))

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
        (service-log :error "Error while creating worker thread for new connection: ~A" c)))))

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
(defclass net-service (service)
  ((port :reader port :initarg :port)
   (address :reader address :initarg :address)
   (request-class :type symbol :initarg :request-class :accessor service-request-class)
   (response-class :type symbol :initarg :response-class :accessor service-response-class)
   (engine :type engine :accessor engine :initarg :engine)
   ;; TODO 2024-12-08: hunchentoot uses read-timeout/write-timeout - figure out if needed
   (timeout :type fixnum :initarg :timeout :accessor service-timeout)
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
   :request-class 'net-request
   :response-class 'response
   :timeout *default-connection-timeout*
   :logger (make-instance 'service-logger :message-log-output *error-output* :access-log-output *error-output*)
   :backlog -1 ;; TODO 2024-10-23: what is a correct initial value here? wookie uses -1
   :request-count 0
   :shutdown-p t
   :shutdown-lock (sb-thread:make-mutex :name "shutdown-lock")
   :shutdown-queue (sb-thread:make-waitqueue :name "shutdown-queue"))
  (:documentation "The service class is designed primarily for webservers and functionally
similar to HUNCHENTOOT:ACCEPTOR."))

(defaccessor name ((self net-service)) (id:id self))

(defmethod message-log-output ((self net-service))
  (message-log-output (logger self)))

(defmethod access-log-output ((self net-service))
  (access-log-output (logger self)))

(defmethod print-object ((self net-service) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A on port ~A"
            (or (address self) "*") (port self))))

(defaccessor sesion-db ((self net-service)) *session-db*)

(defmethod service-log-message ((self net-service) level format-string &rest args)
  (log:with-log-stream (stream (message-log-output self) *message-log-lock*)
    (handler-case
        (format stream "[~A~@[ ~A~]] ~?~%"
        (obj/time:iso-time) level
        format-string args)
      (error (e)
        (ignore-errors
         (format *trace-output* "error ~A while writing to error log, error not logged~%" e))))))

(defun service-log (level format-string &rest args)
  (apply 'service-log-message *service* level format-string args))

(defmethod start-listening :around ((self net-service))
  (when (socket self)
    (simple-service-error "service ~A is already listening" self))
  ;; setup the socket and call SOCKET-LISTEN
  (call-next-method)
  (when (zerop (port self))
    (setf (slot-value self 'port) (nth-value 1 (socket-name (socket self))))))

(defun socket-bind* (self)
  (restart-case
      (socket-bind (socket self)
                   (or (address self)
                       #(0 0 0 0))
                   (port self))
    (get-port-from-os () 
      (setf (slot-value self 'port) 0)
      (socket-bind* self))))

(defmethod start-listening ((self net-service))
  (unless (socket self)
    (setf (socket self) (make-instance 'inet-socket :type :stream :protocol :tcp)))
  (socket-bind* self)
  (socket-listen (socket self)
                 (backlog self))
  (values))

(defmacro with-open-socket ((var socket) &body body)
  "Bind SOCKET to VAR and eval BODY followed by calling SOCKET-CLOSE on SOCKET."
  (once-only (socket)
    `(let ((,var ,socket))
       (unwind-protect (when ,var ,@body)
         (when ,var (socket-close ,var))))))
       
(defmethod accept ((self net-service))
  (with-open-socket (sock (socket self))
    (loop
      (with-mutex ((shutdown-lock self))
        (when (shutdown-p self)
          (return))
        (when (print (socket-listen sock (backlog self))))
          (when-let ((conn
                      (handler-case (socket-accept sock)
                        (sb-bsd-sockets::connection-refused-error ()))))
            (setf (sb-impl::fd-stream-timeout (socket-make-stream conn :input t))
                  (coerce (service-timeout self) 'single-float))
            (handle-connection (engine self) conn))))))

;; (defmethod dispatch-request ((self service) request))

;; (defmethod service-status-message )

(defmethod start ((self net-service))
  (setf (shutdown-p self) nil)
  (setq *service* self)
  (std:println *service*)
  (service-log :info "starting service ~A" (name self))
  (let ((engine (engine self)))
    (service-log :debug "using engine ~A" engine)
    (setf (service engine) self)
    (start-listening self)
    (exec engine))
  self)

(defmethod started-p ((self net-service))
  (and (socket self) t))
                        
(defmethod stop :around ((self net-service) &key graceful)
  (with-mutex ((shutdown-lock self))
    (setf (shutdown-p self) t)
    (call-next-method)
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
      (warn 'protocol-warning :message "service socket unbound"))
    self))

(defmethod initialize-connection-hook ((self net-service) stream)
  stream)

(defmethod process-connection :around ((*service* net-service) (socket t))
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

(defmethod remove-session-hook ((service net-service) (session t))
  nil)

(defmethod service-make-request (service socket &rest args &key &allow-other-keys)
  "Make a REQUEST instance for SERVICE."
  (multiple-value-bind (raddr rport)
      (std:if-let ((remote (getf args :remote)))
        (values-list remote)
        (sb-bsd-sockets:socket-peername socket))
    (multiple-value-bind (laddr lport)
        (std:if-let ((local (getf args :local)))
          (values-list local)
          (socket-name socket))
      (apply 'make-instance (service-request-class service)
             :service service
             :local-addr laddr
             :local-port lport
             :remote-addr raddr
             :remote-port rport
             args))))


(defgeneric detach-socket (self)
  (:method ((self net-service))
    (setf *finish-processing-socket* t
          *close-service-stream* nil)))

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
