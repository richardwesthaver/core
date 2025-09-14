;;; net/srv/proto.lisp --- Lisp Net Services

;;

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

;;; Vars
(defvar *router*)
(defvar *session-db* nil)
(defvar *routes* '(dispatch-routes))
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
(defconstant +handler-tag+ 'handler-tag)
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

(std:definline register-service (name srv)
  (setf (gethash name *service-table*) srv))

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
(defgeneric service-log-message (self level format-string &rest arguments))
(defgeneric service-log-access (self &optional code))
(defgeneric process-request (req)
  (:documentation "Function called by PROCESS-CONNECTION after reading incoming headers. Calls
HANDLE-REQUEST to dispatch to a route and return output to the client using
START-OUTPUT.

Return value is ignored."))

;;; Conditions
(defun abort-request-handler (&optional result)
  "This function can be called by a request handler at any time to
immediately abort handling the request.  This works as if the handler
had returned RESULT.  See the source code of REDIRECT for an example."
  (throw '#.+handler-tag+ result))

;;; Config
(defconfig net-service-config (service-config) ()
  (:default-initargs
   :request-class 'net-service-request
   :response-class 'net-service-response))

(defmethod make-config ((self (eql :net)) &rest args)
  (apply 'make-instance 'net-service-config args))

(defmethod load-config ((self (eql :net)) (from t) &key)
  (apply 'make-config (std/file::file-read-forms from)))

;;; Classes
(defclass net-response (response) ())

(defclass net-service-response (net-response service-response) ())

(defclass net-request (request)
  ((local-addr :initarg :local-addr :reader local-addr)
   (local-port :initarg :local-port :reader local-port)
   (remote-addr :initarg :remote-addr :reader remote-addr)
   (remote-port :initarg :remote-prot :reader remote-port)))

(defclass net-service-request (net-request service-request) ())

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
;; TODO 2025-09-12: kernels?
(defclass single-threaded-engine (engine) ())

(defmethod handle-connection ((self single-threaded-engine) socket)
  (process-connection (service self) socket))

(defun too-many-engine-requests (self)
  (service-log-message 
   (service self)
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

;; Multithreaded runtime for services
(defclass multi-threaded-engine (engine supervisor) ()
  (:default-initargs :thread nil)
  (:documentation "A multi-threaded ENGINE with a dedicated thread. This class is technically a
SUPERVISOR where the SCOPE is bound to a value based on the current SERVICE at
runtime (a call to RUN-THREAD)."))

(defaccessor name ((self multi-threaded-engine)) (thread-name (supervisor-thread self)))

(defmethod run-thread ((self multi-threaded-engine) thunk &key name scope)
  (when scope (setf (slot-value self 'scope) scope))
  (setf (supervisor-thread self) (make-thread thunk :name name)))

(defmethod handle-connection ((self multi-threaded-engine) socket)
  (run-thread self (lambda () (process-connection (service self) socket))))

(defmethod exec ((self multi-threaded-engine))
  "Execute the engine SELF which is assumped to have a bound SERVICE slot. ACCEPT
is called on the service in a separate supervisor thread."
  (run-thread 
   self
   (lambda () (accept (service self)))
   :name (format nil "~A ~A ~A"
                 (name (service self))
                 (or (address (service self)) "*")
                 (port (service self)))
   :scope (service self))
  (values))

(defmethod stop ((self multi-threaded-engine) &key)
  "Stop the engine SELF by joining its THREAD if it exists, else return NIL."
  (when-let ((th (supervisor-thread self)))
    (join-thread th)))
                    
;; default class
(defclass thread-per-connection-engine (multi-threaded-engine thread-pool)
  ((max-accept-count
    :type (or integer null)
    :initarg :max-accept-count
    :initform nil
    :accessor max-accept-count)
   (accept-count
    :type counter
    :initform (std:make-counter)
    :accessor accept-count)
   ;; (accept-count-lock
   ;;  :initform (make-mutex :name "accept-count")
   ;;  :reader accept-count-lock)
   (wait-queue
    :initform (sb-concurrency:make-queue)
    :reader wait-queue)
   (wait-lock
    :initform (make-mutex :name "wait-queue")
    :reader wait-lock))
  (:default-initargs
   :limiter-count *default-max-thread-count*
   :workers #() ;; workers are initialized so that WORKER-COUNT may be used
   :max-accept-count *default-max-accept-count*))

(defmethod initialize-instance :after ((self thread-per-connection-engine) &rest args)
  "Ensure MAX-ACCEPT-COUNT > LIMITER-COUNT."
  (declare (ignore args))
  (when (max-accept-count self)
    (unless (limiter-count self)
      (error "LIMITER-COUNT must be supplied if MAX-ACCEPT-COUNT is."))
    (unless (> (max-accept-count self) (limiter-count self))
      (error "MAX-ACCEPT-COUNT must be greater than LIMITER-COUNT"))))

(std:definline increment-accept-count (eng)
  (std:inc-counter (accept-count eng)))

(std:definline decrement-accept-count (eng)
  (std:dec-counter (accept-count eng)))

(defun wait-for-free-connection (self)
  "Wait until a connection is available (< WORKER-COUNT LIMITER-COUNT)."
  (declare (thread-per-connection-engine self))
  (with-mutex ((wait-lock self))
    (loop until (< (worker-count self) (limiter-count self))
          do (sb-thread:condition-wait (wait-queue self) (wait-lock self)))))

(defmethod handle-connection ((self thread-per-connection-engine) socket)
  (increment-accept-count self)
  (flet ((pconn (service socket)
           (update-limiter-count self 1)
           (unwind-protect (process-connection service socket)
             (update-limiter-count self -1))))
    (cond ((null (limiter-count self))
           (process-connection (service self) socket))
          ((if (max-accept-count self)
               (>= (accept-count self) (max-accept-count self))
               (>= (worker-count self) (limiter-count self)))
           (too-many-engine-requests self)
           (send-service-unavailable-response self socket))
          ((and (max-accept-count self)
                (>= (worker-count self) (limiter-count self)))
           (wait-for-free-connection self)
           (pconn (service self) socket))
          (t
           (pconn (service self) socket)))))

(defmethod create-request-worker-thread ((self thread-per-connection-engine) socket)
  "Create a thread which handles a request from SOCKET."
  (handler-case
      (run-thread
       self
       (lambda () (handle-connection self socket))
       :name (format nil "worker:~A" (socket-peername socket)))
    (error (c)
      (let ((*service* (service self)))
        (ignore-errors
         (close (socket-make-stream (socket *service*)) :abort t))
        (service-log :error "Error while creating worker for new connection: ~A" c)))))

;; supervisor, worker, task, kernel

;;; Service
(defclass net-service (service server)
  ((port :accessor port :initarg :port)
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
   (shutdown-queue :type sb-thread:waitqueue :accessor shutdown-queue :initarg :shutdown-queue))
  (:default-initargs
   :id (symbol-name (gensym "service"))
   :port *default-service-port*
   :engine (make-instance 'thread-per-connection-engine)
   :address *localhost*
   :request-class 'net-service-request
   :response-class 'net-service-response
   :timeout *default-connection-timeout*
   :logger (make-instance 'service-logger :message-log-output *error-output* :access-log-output *error-output*)
   :backlog -1 ;; TODO 2024-10-23: what is a correct initial value here? wookie uses -1
   :request-count 0
   :shutdown-p t
   :shutdown-lock (sb-thread:make-mutex :name "shutdown-lock")
   :shutdown-queue (sb-thread:make-waitqueue :name "shutdown-queue"))
  (:documentation "The service class is designed primarily for webservers and functionally
similar to HUNCHENTOOT:ACCEPTOR."))

(defmethod shared-initialize :after ((self net-service) slots &key port address name)
  (when name (register-service name self))
  (when (consp port) ; assumed to be a port range - we select one at random, ensure it is free, and replace
    (destructuring-bind (lo . hi) port
      (setf (port self) (find-port :min (+ lo (random (- hi lo))) :max hi :host (or address *localhost*))))))

(defaccessor name ((self net-service)) (id:id self))

(defmethod alive ((self net-service)) (not (shutdown-p self)))
(defmethod (setf alive) (new (self net-service)) (setf (shutdown-p self) (not new)))

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
                        
(defmethod stop ((self net-service) &key graceful)
  (sb-thread:with-recursive-lock ((shutdown-lock self))
    (setf (shutdown-p self) t)
    (when graceful
      (when (plusp (request-count self))
        (sb-thread:condition-wait (shutdown-queue self)
                                  (shutdown-lock self)))))
  (stop (engine self))
  (std:if-let ((sock (socket self)))
    (progn
      (sb-bsd-sockets:socket-close sock)
      (setf (socket self) nil))
    (warn 'protocol-warning :message "service socket unbound"))
  self)

(defmethod initialize-connection-hook ((self net-service) stream)
  stream)

(defmethod process-connection :around ((*service* net-service) (socket t))
  (with-logger *service*
    ;; (with-conditions-caught-and-logged ()
    ;; (with-mapped-conditions ()
    (call-next-method))) ;; )

(defun call-with-request-count-incf (*service* function)
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
  `(call-with-request-count-incf ,service (lambda () ,@body)))

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
(defmacro defservice (name super slots &rest opts)
  "Define a subclass of NET/SRV:SERVICE."
  `(defclass ,name ,(or super '(service)) ,slots ,@opts))

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

  (var &key real-name parameter-type init-form request-type)."
  (declare (ignore spec args body)))

(defun dispatch-routes (req)
  "A dispatcher which returns the appropriate handler defined with DEFROUTE, if
available."
  (loop for (uri routes router host) in *routes*
        when (and (or (eq routes t)
                      (find (name *service*) routes :test #'eq))
                  (cond ((stringp uri)
                         (and (or (null host)
                                  (string= (or (address req) "unknown")
                                           host))
                              ;; Support RE for matching host names as well (wildcards)?
                              ;; (string= (script-name req) uri) ; http-request only
                              ))
                        (t (funcall uri req))))
        do (return router)))
  
(defmacro with-service ((name) &body body)
  `(let ((*service* (gethash ,name *service-table*)))
     ,@body))
