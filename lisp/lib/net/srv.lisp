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

(srv:define-service my-homepage (:port 8080
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

;;; Vars
(defvar *router*)
(defvar *service*)
(defvar *handlers*)
(defvar-unbound *request*)
(defvar-unbound *response*)
(defvar-unbound *session*)
(defvar-unbound *session-secret*)
(defvar-unbound *service-stream*)
(defvar-unbound *finish-processing-socket*)
(defvar-unbound *close-service-stream*)
(defun in-request-p () (and (boundp '*request*) *request*))
(defun in-response-p () (and (boundp '*response*) *response*))
(defvar *session-db* nil)
(defvar *global-session-db-lock* (load-time-value (make-mutex :name "global-session-db")))
(defvar *access-log-lock* (make-mutex :name "access-log"))
(defvar *message-log-lock* (make-mutex :name "message-log"))
(defvar *default-connection-timeout* 20)
(defvar *default-connection-max* 16)
(defvar *default-service-port* 8080)
(defvar *default-max-thread-count* 100)
(defvar *default-max-accept-count* (+ *default-max-thread-count* 20))
#+ssl (defvar *default-ssl-service-port* 8443)
(defvar *default-session-timeout* #.(* 30 60)) ;; 30m
(defvar *default-content-type* "text/html")
;;; Conditions
;; from hunchentoot
(define-condition service-condition (condition) ())
(eval-always
  (deferror service-error (service-condition error) () (:auto t)))
(deferror service-simple-error (service-error simple-condition) () (:auto t))

(define-condition service-warning (service-condition warning) ())

(defwarning service-simple-warning (service-warning simple-condition) () (:auto t))

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

;;; Protocol
(defgeneric start-listening (self))
(defgeneric service-status-message (service status-code &key &allow-other-keys))
(defgeneric restart-service (self)
  (:documentation "Restart a service.")
  (:method ((self t))
    (stop self)
    (start self)))
(defgeneric execute-service (self)
  (:documentation "A function that is called by a service once it has been initialized. Usually calls the 'accept-connections' method of the service."))
(defgeneric add-route (self uri handler &key &allow-other-keys))
(defgeneric delete-route (self uri &key &allow-other-keys))
(defgeneric handle-request (self request)
  (:documentation "Function called after fetching a request. Used to establish error handling,
logging, etc."))
(defgeneric dispatch-request (self request)
  (:documentation "Function called after 'handle-request' which routes a request to a handler."))
(defgeneric service-name (self)
  (:method ((self t))
    (obj/id:id self)))
(defgeneric accept-connections (self))
(defgeneric initialize-connection-hook (self stream))
(defgeneric reset-connection-stream (self stream))
(defgeneric process-connection (self socket))
(defgeneric secure-service-p (self)
  (:method ((self t)) 
    (declare (ignore self))
    nil))

(defun ssl-p (&optional (service *service*))
  (and (secure-service-p service)
       (eql :https (socket-protocol (socket service)))))

(defgeneric service-log-message (self level format-string &rest arguments))
(defgeneric service-log-access (self &optional code))

;;; Response
(defclass response () ())
(defclass http-service-response (response) ((response :type http-response)))

;; content-type
;; content-length *
;; headers-out
;; return-code * status-code
;; external-format //
;; cookies-out
;;; Request
(defclass request ()
  ((service :initarg :service
           :reader request-service)
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

;; method
;; uri
;; headers-in
;; cookies-in
;; get-parameters
;; post-parameters
;; script-name
;; query-string
;; raw-post-data

(defclass http-service-request (request)
  ((request :type http-request)))

;;; Session

(defgeneric session-db-lock (service &optional global)
  (:method ((service t) &optional (global t))
    (declare (ignore global))
    *global-session-db-lock*))

(defmacro with-session-db-lock (lock &body body)
  (with-gensyms (th)
    (once-only (lock)
      `(flet ((,th () ,@body))
         (cond (,lock (with-mutex (,lock) (,th)))
               (t (,th)))))))

(defgeneric remove-session-hook (service session))

(defgeneric session-db (service)
  (:method ((service t))
    *session-db*))

(defgeneric (setf session-db) (new service)
  (:method (new (service t))
    (setq *session-db* new)))

(defgeneric next-session-id (service))

(let ((session-id-counter 0))
  (defmethod next-session-id ((service t))
    (incf session-id-counter)))

;; HACK 2024-07-18: currently not storing the SESSION-STRING directly in this
;; class as a slot. may need to change but I would rather have the string
;; cached/displaced to some other location.. depends how often we need that
;; string.

;; session-cookie-value, session-verify

(defclass session (obj/id:id)
  ((id :type integer :initarg :id)
   (user-agent :reader user-agent :initarg :user-agent)
   (remote-addr :reader remote-addr :initarg :remote-addr)
   (session-start :reader session-start)
   (last-click :reader last-click :initarg :last-click)
   (data :reader session-data :initarg :data)
   (timeout :type fixnum :reader session-timeout :initarg :timeout))
  (:default-initargs
   :session-start (get-universal-time)
   :last-click (get-universal-time)
   :timeout *default-session-timeout*))

(defun remove-session (session)
  "Remove SESSION from the global session database."
  (with-session-db-lock (session-db-lock *service*)
    (remove-session-hook *service* session)
    (setf (session-db *service*)
          (delete (id:id session) (session-db *service*)
                  :key #'car :test #'=))))

(defun reset-session-secret ()
  (setq *session-secret* (random-chars 32)))

(defvar *session-encode-user-agent* t)
(defvar *session-encode-remote-addr* nil)

(defun encode-session-string (id &optional user-agent remote-addr (start 0))
  (unless (boundp '*session-secret*)
    (service-simple-warning "Session secret is unbound.  Using Lisp's RANDOM function to initialize it.")
    (reset-session-secret))
  ;; *SESSION-SECRET* is used twice due to known theoretical
  ;; vulnerabilities of MD5 encoding
  (sb-md5:md5sum-string 
   (concatenate 'string
                *session-secret*
                (sb-md5:md5sum-string 
                 (format nil "~A~A~@[~A~]~@[~A~]~A"
                         *session-secret*
                         id
                         (and *session-encode-user-agent*
                              user-agent)
                         (and *session-encode-remote-addr*
                              remote-addr)
                         start)))))

(defun stringify-session (session)
  "Create a string representation of a SESSION object."
  (encode-session-string (id:id session)
                         (user-agent session)
                         (remote-addr session)
                         (session-start session)))

(defun session-expired-p (session)
  (< (+ (last-click session) (session-timeout session))
     (get-universal-time)))

(defun session-gc ()
  "Removes sessions from the global session database which have expired or are invalid."
  (with-session-db-lock (session-db-lock *service*)
    (setf (session-db *service*)
          (loop for pair in (session-db *service*)
                for (nil . session) = pair
                when (session-expired-p session)
                do (remove-session-hook *service* session)
                else
                collect pair)))
  (values))

(defun get-session (id)
  (let ((session
          (cdr (assoc id (session-db *service*) :test #'=))))
    (when (and session
               (session-expired-p session))
      (when *response*
        (log-message* :info "Session with ID ~A too old" id))
      (remove-session session)
      (setq session nil))
    session))

(defun session-value (sym &optional (session *session*))
  (when session
    (let ((found (assoc sym (session-data session) :test #'eq)))
      (values (cdr found) found))))

(defsetf session-value (sym &optional session) (new-val)
  (once-only (sym)
    (with-gensyms (place %session)
      `(let ((,%session (or ,session (start-session))))
         (with-session-db-lock ((session-db-lock *service* nil))
           (let* ((,place (assoc ,sym (session-data ,%session) :test #'eq)))
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
          (delete sym (session-data session)
                  :key 'car :test 'eq))))

(defgeneric session-cookie-value (session)
  (:method ((session session))
    (and session
         (format nil
                 "~D:~A"
                 (id:id session)
                 (stringify-session session)))))

(defvar *session-gc-frequency* 60)

(defgeneric session-created (service new-session))

(let ((session-usage-counter 0))
  (defmethod session-created ((service t) (session t))
    (when (and *session-gc-frequency*
               (zerop (mod (incf session-usage-counter)
                           *session-gc-frequency*)))
      (session-gc))))

(defun start-session ()
  (let ((session (session *request*)))
    (when session
      (return-from start-session session))
    (with-session-db-lock (session-db-lock *service*)
      (setf session (make-instance 'session))
      (setf (session *request*) session
            (session-db *service*)
            (acons (id:id session) session (session-db *service*))))
    #+nil
    (set-cookie (session-cookie-name *acceptor*)
                :value (session-cookie-value session)
                :path "/"
                :http-only t)
    (session-created *service* session)
    (setq *session* session)))

(defun refresh-session-cookie-value (session)
  (setf (slot-value session 'session-start) (get-universal-time)
        (slot-value session 'session-string) (stringify-session session))
  #+nil
  (set-cookie (session-cookie-name *service*)
              :value (session-cookie-value session)
              :path "/"
              :http-only t))

(defgeneric session-verify (request))

(defun reset-sessions (&optional (service *service*))
  (with-session-db-lock (session-db-lock service)
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

;;; Router
;; similar to HUNCHENTOOT:EASY-HANDLER
(defclass router () ())

;;; Engine
;; Multithreaded runtime for services

(define-task-kernel service-task-kernel () ()
  "Default task kernel for service-based tasks.")

(defclass engine () ((service :accessor engine-service)))

(defclass single-threaded-engine (engine) ())

(defclass multi-threaded-engine (engine)
  ((process :accessor service-process)))

(defclass thread-per-connection-engine (multi-threaded-engine)
  ((max-thread-count
    :type (or integer null)
    :initarg :max-thread-count
    :initform nil
    :accessor engine-max-thread-count)
   (thread-count
    :type integer
    :initform 0
    :accessor engine-thread-count)
   (thread-count-lock
    :initform (make-mutex :name "engine-thread-count")
    :accessor engine-thread-count-lock)
   (max-accept-count
    :type (or integer null)
    :initarg :max-accept-count
    :initform nil
    :accessor engine-max-accept-count)
   (accept-count
    :type integer
    :initform 0
    :accessor engine-accept-count)
   (accept-count-lock
    :initform (make-mutex :name "engine-accept-count"))
   (wait-queue
    :initform (sb-concurrency:make-queue)
    :reader engine-wait-lock)
   (worker-thread-name-format
    :type (or string null)
    :initarg :worker-thread-name-format
    :initform "service-worker-~A"
    :accessor engine-worker-thread-name-format))
  (:default-initargs
   :max-thread-count *default-max-thread-count*
   :max-accept-count *default-max-accept-count*))

;; supervisor, worker, task, kernel
;;; Service
(defclass service (obj/id:id)
  ((port :reader service-port :initarg :port)
   (address :reader service-address :initarg :address)
   (request-class :type symbol :initarg :request-class :accessor service-request-class)
   (response-class :type symbol :initarg :response-class :accessor service-response-class)
   (engine :type service-engine :accessor service-engine :initarg :engine)
   (read-timeout :type fixnum :initarg :read-timeout)
   (write-timeout :type fixnum :initarg :write-timeout)
   (connection-max :type (or fixnum null) :initarg :connection-max)
   (logger :type service-logger :initarg :logger :reader logger)
   ;; RESEARCH 2024-07-18: 
   ;; may need to start dealing with this
   ;; https://datatracker.ietf.org/doc/html/rfc2616#section-3.6.1
   (chunk-output-p :type boolean :initarg :chunk-output-p)
   (chunk-input-p :type boolean :initarg :chunk-input-p)
   (socket :type socket :accessor socket :initarg :socket)
   (backlog :accessor backlog :initarg :backlog)
   (request-count :type integer :accessor request-count :initarg :request-count)
   (shutdown-p :type boolean :accessor shutdown-p :initarg :shutdown-p)
   (shutdown-lock :type mutex :accessor shutdown-lock :initarg :shutdown-lock)
   (shutdown-queue :type waitqueue :accessor shutdown-queue :initarg :shutdown-queue))
  (:default-initargs
   :id (symbol-name (gensym "service"))
   :port *default-service-port*
   :engine (make-instance 'thread-per-connection-engine)
   :address nil
   :request-class 'service-request
   :response-class 'service-response
   :chunk-output-p t
   :chunk-input-p t
   :read-timeout *default-connection-timeout*
   :write-timeout *default-connection-timeout*
   :connection-max *default-connection-max*
   :logger (make-instance 'service-logger)
   :request-count 0
   :shutdown-p t
   :shutdown-lock (sb-thread:make-mutex :name "shutdown-lock")
   :shutdown-queue (sb-thread:make-waitqueue :name "shutdown-queue"))
  (:documentation "The service class is designed primarily for webservers and functionally
similar to HUNCHENTOOT:ACCEPTOR."))

(defmethod message-log-output ((self service))
  (message-log-output (logger self)))

(defmethod access-log-output ((self service))
  (access-log-output (logger self)))

(defmethod print-object ((self service) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A on port ~A"
            (or (service-address self) "*") (service-port self))))

(defmethod service-log-message ((self service) level format-string &rest args)
  (log:with-log-stream (stream (message-log-output self) *message-log-lock*)
    (handler-case
        (format stream "[~A~@[ [~A]~]] ~?~%"
        (obj/time:iso-time) level
        format-string args)
      (error (e)
        (ignore-errors
         (format *trace-output* "error ~A while writing to error log, error not logged~%" e))))))

(defmethod service-log-access ((self service) &optional code)
  "Default method for access logging.  It logs the information to the
destination determined by (ACCESS-LOG-OUTPUT SERVICE) in a format that
can be parsed by most log analysis tools."
  (log:with-log-stream (stream (access-log-output self) *access-log-lock*)
    (format stream "~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~] ~:[-~;~:*~A~] [~A] \"~A ~A~@[?~A~] ~
                    ~A\" ~D ~:[-~;~:*~D~] \"~:[-~;~:*~A~]\" \"~:[-~;~:*~A~]\"~%"
            (remote-addr*)
            (header-in* :x-forwarded-for)
            (authorization)
            (iso-time)
            (request-method*)
            (script-name*)
            (query-string*)
            (server-protocol*)
            code
            (content-length*)
            (referer)
            (user-agent))))

(defun log-message* (level format-string &rest args)
  (apply 'service-log-message *service* level format-string args))

(defmethod start-listening ((self service))
  (when (socket self)
    (service-simple-error "service ~A is already listening" self))
  (setf (socket self)
        (socket-listen (socket-bind (make-instance 'inet-socket :type :stream :protocol :tcp)
                                    (or (service-address self)
                                        #(0 0 0 0))
                                    (service-port self))
                       (backlog self)))
  (values))

;; (defmethod accept-connections ((self service)))

;; (defmethod dispatch-request ((self service) request))

;; (defmethod handle-request ((*service* service) (*request* request)))

;; (defmethod service-status-message )

(defmethod start ((self service))
  (setf (shutdown-p self) nil)
  (let ((engine (service-engine self)))
    (setf (engine-service engine) self)
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
    (shutdown (service-engine self))
    (sb-bsd-sockets:socket-close (socket self))
    (setf (socket self) nil)
    self))

(defmethod initialize-connection-hook ((self service) stream)
  stream)

(defmethod reset-connection-stream ((self service) stream)
  (cond ((typep stream 'chunga:chunked-stream)
         (setf (chunga:chunked-stream-output-chunking-p stream) nil
               (chunga:chunked-stream-input-chunking-p stream) nil)
         (chunga:chunked-stream-stream stream))
         (t stream)))

#+nil
(defmethod process-connection :around ((*service* service) (socket t))
  (with-conditions-caught-and-logged ()
    (with-mapped-conditions ()
      (call-next-method))))

(defun do-with-service-request-count-incremented (*service* function)
  (with-mutex ((shutdown-lock *service*))
    (incf (request-count *service*)))
  (unwind-protect
       (funcall function)
    (with-mutex ((shutdown-lock *service*))
      (decf (request-count *service*))
      (when (shutdown-p *service*)
        (sb-thread:condition-broadcast (shutdown-queue *service*))))))

(defmacro with-service-request-count-incremented ((service) &body body)
  "Execute BODY with REQUEST-COUNT of SERVICE
  incremented by one.  If the SHUTDOWN-P returns true after
  the BODY has been executed, the SHUTDOWN-QUEUE condition
  variable of the SERVICE is signalled in order to finish shutdown
  processing."
  `(do-with-acceptor-request-count-incremented ,service (lambda () ,@body)))

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

(defmethod process-connection ((*service* service) (socket t))
  (let* ((socket-stream (sb-bsd-sockets:socket-make-stream socket))
         (*service-stream*)
         (*close-service-stream* t)
         (remote (multiple-value-list (socket-peername socket)))
         (local (multiple-value-list (socket-name socket))))
    (unwind-protect
         (progn
           (setq *service-stream* (initialize-connection-hook *service* socket-stream))
           (loop
             (let ((*finish-processing-socket* t))
               (when (shutdown-p *service*)
                 (return))
                   ;; TODO
               (finish-output *service-stream*)
               (setq *service-stream* (reset-connection-stream *service* *service-stream*))
               (when *finish-processing-socket*
                 (return)
               ))))
      (when *close-service-stream*
        (flet ((close-stream (st)
                 (ignore-errors (finish-output st))
                 (ignore-errors (close st :abort t))))
          (unless (or (not *service-stream*)
                      (eql socket-stream *service-stream*))
            (close-stream *service-stream*))
          (close-stream socket-stream))))))

#+ssl
(defclass ssl-service (service)
  ((certificate-file :initarg :certificate-file
                     :reader certificate-file)
   (privatekey-file :initarg :privatekey-file
                    :reader privatekey-file)
   (privatekey-password :initarg :privatekey-password
                        :reader privatekey-password))
  (:default-initargs
   :password nil
   :port 443))

(defmethod initialize-instance :after ((self ssl-service) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value self 'privatekey-file)
        (namestring (truename (privatekey-file self)))
        (slot-value self 'certificate-file)
        (namestring (truename (certificate-file self)))))

(defmethod secure-service-p ((self ssl-service))
  (declare (ignore self))
  t)

(defmethod initialize-connection-hook ((self ssl-service) stream)
  (call-next-method self
                    (cl+ssl:make-ssl-server-stream
                     stream
                     :certificate (certificate-file self)
                     :key (privatekey-file self)
                     :password (privatekey-password self))))

(defun get-peer-ssl-certificate ()
  (cl+ssl:ssl-stream-x509-certificate *service-stream*))

;;; Macros
(defmacro define-service (name &rest initargs)
  "Define a subclass of NET/SRV:SERVICE."
  `(defclass ,name ,@initargs))
