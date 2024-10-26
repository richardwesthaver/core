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

(pkg:defpkg :net/srv/http
  (:use :cl :std :net/proto/http 
   :net/codec/http :net/core :net/cookie)
  (:use-reexport :net/srv))

(pkg:defpkg :net/srv/udp
  (:use :cl :std :net/udp :net/codec/tlv :net/core)
  (:use-reexport :net/srv))

;;; Vars
(defvar *router*)
(defvar *service*)
(defvar *services*)
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
(eval-always
  (defvar *http-status-message-map* (make-hash-table)
    "Used to map numerical return codes to message strings.")
  (defun http-status-message (i)
    (gethash i *http-status-message-map*)))

;;; Return Codes
(defmacro def-http-return-code (name value message)
  "Shortcut to define constants for return codes.  NAME is a
Lisp symbol, VALUE is the numerical value of the return code, and
MESSAGE is the phrase \(a string) to be shown in the
server's status line."
  `(eval-when (:compile-toplevel :execute :load-toplevel)
     (defconstant ,name ,value ,(format nil "HTTP return code \(~A) for '~A'."
                                        value message))
     (setf (gethash ,value *http-status-message-map*) ,message)))

(def-http-return-code +http-continue+ 100 "Continue")
(def-http-return-code +http-switching-protocols+ 101 "Switching Protocols")
(def-http-return-code +http-processing+ 102 "Processing")
(def-http-return-code +http-ok+ 200 "OK")
(def-http-return-code +http-created+ 201 "Created")
(def-http-return-code +http-accepted+ 202 "Accepted")
(def-http-return-code +http-non-authoritative-information+ 203 "Non-Authoritative Information")
(def-http-return-code +http-no-content+ 204 "No Content")
(def-http-return-code +http-reset-content+ 205 "Reset Content")
(def-http-return-code +http-partial-content+ 206 "Partial Content")
(def-http-return-code +http-multi-status+ 207 "Multi-Status")
(def-http-return-code +http-already-reported+ 208 "Already Reported")
(def-http-return-code +http-im-used+ 226 "IM Used")
(def-http-return-code +http-multiple-choices+ 300 "Multiple Choices")
(def-http-return-code +http-moved-permanently+ 301 "Moved Permanently")
(def-http-return-code +http-moved-temporarily+ 302 "Moved Temporarily")
(def-http-return-code +http-see-other+ 303 "See Other")
(def-http-return-code +http-not-modified+ 304 "Not Modified")
(def-http-return-code +http-use-proxy+ 305 "Use Proxy")
(def-http-return-code +http-temporary-redirect+ 307 "Temporary Redirect")
(def-http-return-code +http-permanent-redirect+ 308 "Permanent Redirect")
(def-http-return-code +http-bad-request+ 400 "Bad Request")
(def-http-return-code +http-authorization-required+ 401 "Authorization Required")
(def-http-return-code +http-payment-required+ 402  "Payment Required")
(def-http-return-code +http-forbidden+ 403 "Forbidden")
(def-http-return-code +http-not-found+ 404 "Not Found")
(def-http-return-code +http-method-not-allowed+ 405 "Method Not Allowed")
(def-http-return-code +http-not-acceptable+ 406 "Not Acceptable")
(def-http-return-code +http-proxy-authentication-required+ 407 "Proxy Authentication Required")
(def-http-return-code +http-request-time-out+ 408 "Request Time-out")
(def-http-return-code +http-conflict+ 409 "Conflict")
(def-http-return-code +http-gone+ 410 "Gone")
(def-http-return-code +http-length-required+ 411 "Length Required")
(def-http-return-code +http-precondition-failed+ 412 "Precondition Failed")
(def-http-return-code +http-request-entity-too-large+ 413 "Request Entity Too Large")
(def-http-return-code +http-request-uri-too-large+ 414 "Request-URI Too Large")
(def-http-return-code +http-unsupported-media-type+ 415 "Unsupported Media Type")
(def-http-return-code +http-requested-range-not-satisfiable+ 416 "Requested range not satisfiable")
(def-http-return-code +http-expectation-failed+ 417 "Expectation Failed")
(def-http-return-code +http-im-a-teapot+ 418 "I'm a teapot")
(def-http-return-code +http-misdirected-request+ 421 "Misdirected Request")
(def-http-return-code +http-unprocessable-entity+ 422 "Unprocessable Entity")
(def-http-return-code +http-locked+ 423 "Locked")
(def-http-return-code +http-failed-dependency+ 424 "Failed Dependency")
(def-http-return-code +http-upgrade-required+ 426 "Upgrade Required")
(def-http-return-code +http-precondition-required+ 428 "Precondition Required")
(def-http-return-code +http-too-many-requests+ 429 "Too Many Requests")
(def-http-return-code +http-request-header-fields-too-large+ 431 "Request Header Fields Too Large")
(def-http-return-code +http-connection-closed-without-response+ 444 "Connection Closed Without Response")
(def-http-return-code +http-unavailable-for-legal-reasons+ 451 "Unavailable For Legal Reasons")
(def-http-return-code +http-client-closed-request+ 499 "Client Closed Request")
(def-http-return-code +http-internal-server-error+ 500 "Internal Server Error")
(def-http-return-code +http-not-implemented+ 501 "Not Implemented")
(def-http-return-code +http-bad-gateway+ 502 "Bad Gateway")
(def-http-return-code +http-service-unavailable+ 503 "Service Unavailable")
(def-http-return-code +http-gateway-time-out+ 504 "Gateway Time-out")
(def-http-return-code +http-version-not-supported+ 505 "Version not supported")
(def-http-return-code +http-variant-also-negotiates+ 506 "Variant Also Negotiates")
(def-http-return-code +http-insufficient-storage+ 507 "Insufficient Storage")
(def-http-return-code +http-loop-detected+ 508 "Loop Detected")
(def-http-return-code +http-not-extended+ 510 "Not Extended")
(def-http-return-code +http-network-authentication-required+ 511 "Network Authentication Required")
(def-http-return-code +http-network-connect-timeout-error+ 599 "Network Connect Timeout Error")

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
(defgeneric find-route (self uri))
(defgeneric add-route (self uri srv &key &allow-other-keys))
(defgeneric delete-route (self uri &key &allow-other-keys))
(defgeneric handle-request (self request)
  (:documentation "Function called after fetching a request. Used to establish error handling,
logging, etc."))
(defgeneric dispatch-request (self request)
  (:documentation "Function called after 'handle-request' which routes a request to a service."))
(defgeneric accept-connections (self))
(defgeneric handle-connection (self conn))
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
    (simple-service-warning "Session secret is unbound.  Using Lisp's RANDOM function to initialize it.")
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
(defclass router (pipe) ())

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
   (timeout :type fixnum :initarg :timeout :accessor timeout)
   (connection-max :type (or fixnum null) :initarg :connection-max)
   (logger :type service-logger :initarg :logger :reader logger)
   ;; RESEARCH 2024-07-18: 
   ;; may need to start dealing with this
   ;; https://datatracker.ietf.org/doc/html/rfc2616#section-3.6.1
   (chunk-output-p :type boolean :initarg :chunk-output-p)
   (chunk-input-p :type boolean :initarg :chunk-input-p)
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
   :request-class 'service-request
   :response-class 'service-response
   :chunk-output-p t
   :chunk-input-p t
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
    (simple-service-error "service ~A is already listening" self))
  (setf (socket self) (make-instance 'inet-socket :type :stream :protocol :tcp))
  (socket-bind (socket self)
               (or (address self)
                   #(0 0 0 0))
               (port self))
  (socket-listen (socket self)
                 (backlog self))
  (values))

(defmethod start-listening :after ((self service))
  (when (zerop (port self))
    (setf (slot-value self 'port) (nth-value 1 (socket-name (socket self))))))

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
    (sb-bsd-sockets:socket-close (socket self))
    (setf (socket self) nil)
    self))

(defmethod initialize-connection-hook ((self service) stream)
  stream)

(defmethod reset-connection-stream ((self service) stream)
  (cond ((typep stream 'chunked-stream)
         (setf (output-chunking-p stream) nil
               (input-chunking-p stream) nil)
         (stream-of stream))
         (t stream)))

(defmethod process-connection :around ((*service* service) (socket t))
  (with-logger (*service*)
    ;; (with-conditions-caught-and-logged ()
    ;; (with-mapped-conditions ()
    (call-next-method))) ;; )

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

;;; Http Service
(defclass http-service (service) ())

(defun send-http-response (service stream status-code 
                           &key headers cookies content)
  "Send a HTTP response to STREAM and log it with SERVICE.

STATUS-CODE is the HTTP status code used in the response, HEADERS and COOKIES are used to generate the header. If CONTENT is provided, it is used as the body.

Headers are written to *HEADER-STREAM* when non-nil. 

Returns STREAM."
  (when content
    (setf (content-length*) (length content)))
  (when (content-length*)
    (if (assoc :content-length headers)
        (setf (cdr (assoc :content-length headers)) (content-length*))
        (push (cons :content-length (content-length*)) headers)))
  (service-log-access service status-code)
  (raw-post-data :force-binary t)
  ;; TODO flexi-stream with latin-1?
  (let* ((hstream stream))
    (format hstream "HTTP/1.1 ~D ~A~C~C" status-code (http-status-message status-code) #\Return #\Linefeed)
    (loop for (k . v) in headers
          when v
          do (write-header-line (string-upcase k) v hstream))
    ;; cookies
    (loop for (nil . cookie) in cookies
          do (write-header-line "Set-Cookie" (stringify-cookie cookie) hstream))
    (format hstream "~C~C" #\Return #\Linefeed))
  (when content
    (write-sequence content stream)
    (finish-output stream))
  stream)

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
