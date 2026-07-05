;;; srv.lisp --- Service Protocol

;; Base Protocol used by any type of managed/stateful service.

;;; Commentary:

;; As NET/SRV started coming together I realized we need proper isolation of
;; the implementations (UDP, HTTP/S, and EXTernal at TOW) from the core
;; protocol.

;; This package provides as much common functionality as possible and may be
;; further extended by the implementations.

;; Notably, this package does not perform any IO itself, that is totally up to
;; the implementation. The objects in this package consume incoming packets,
;; requests, and events via HANDLE-* functions.

;;;; TODO:

;; %service-protocol

;; endpoint? closer to service
;; transport? closer to socket
;; routes? build on std/pipe, probably in net/srv/*

;; configs for everything

;; program-service -> sb-ext:run-program IO

;;;; REFS:

;; [[https://github.com/tower-rs/tower][tower]]

;; [[https://github.com/tokio-rs/axum][axum]]

;;; Code:
(in-package :obj/srv)

;;; Vars
(defvar *service* nil)
(defvar *service-table* (make-hash-table))
(defvar *request* nil)
(defvar *response* nil)

;;; Utils
(defun in-request-p () (and (boundp '*request*) *request*))
(defun in-response-p () (and (boundp '*response*) *response*))

(defun find-service (name)
  (gethash name *service-table*))
(defun (setf find-service) (new name)
  (setf (gethash name *service-table*) new))

(std:definline register-service (name srv)
  (setf (find-service name) srv))

;;; Conditions
(define-condition service-condition (condition) ())
(eval-always
  (deferror service-error (service-condition error) () (:reporter t)))
(deferror simple-service-error (service-error simple-condition) () (:reporter t))

(define-condition service-warning (service-condition warning) ())

(defwarning simple-service-warning (service-warning simple-warning) () (:reporter t))

(deferror bad-request (service-error) ())

;;; Objects
(defclass engine ()
  ((service :accessor service :initarg :service 
            :documentation "A link to the SERVICE which owns this instance."))
  (:documentation "An engine provides an execution context for a SERVICE. Engines are responsible
for managing the work done by a service and distributing work to compute
resources. Different engines may use the main thread for execution, a
dedicated thread, their own THREAD-POOL, or a combination of threading
strategies."))

;; TODO 2025-09-12: kernels?
(defclass single-threaded-engine (engine) ())

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

(defclass service (id)
  ((request-class :type symbol :initarg :request-class :accessor service-request-class)
   (response-class :type symbol :initarg :response-class :accessor service-response-class)
   (engine :type engine :accessor engine :initarg :engine))
  (:documentation "Base service class.
A service must specify the request and response classes it uses for
communication as well as the engine which drives it."))

(defclass response () ()
  (:documentation "Base class for response objects, usually generated in reply to a REQUEST."))

(defmethod initialize-instance :after ((self service) &key name &allow-other-keys)
  (when name (register-service name self)))

(defclass service-response (response)
  ((content-type :reader content-type)
   (content-length :reader content-length :initform nil)))

(defclass request ()
  ((data :initarg :data :accessor data))
  (:documentation "Base class for request objects, often paired with RESPONSE objects."))

(defclass service-request (request)
  ((content-stream :initarg :content-stream :reader content-stream)
   (service :initarg :service
	    :reader service)
   (session :initform nil
	    :accessor session)
   (protocol :initarg :request-protocol :reader request-protocol))
  (:documentation "Generic service request."))

;;; Protocol
(defgeneric service (self)
  (:method ((self t)) (when (boundp '*service*) *service*))
  (:method ((self symbol)) (gethash self *service-table*))
  (:method ((self string)) (gethash (symbolicate (string-upcase self)) *service-table*)))

(defgeneric restart-service (self)
  (:documentation "Restart a service.")
  (:method ((self t))
    (stop self)
    (start self)))

(defgeneric handle-request (self request)
  (:documentation "Function called after fetching a request. Used to establish error handling,
logging, etc."))

(defgeneric dispatch-request (self request)
  (:documentation "Function called after 'handle-request' which routes a request to a service."))

(defgeneric send-response (service stream &key content &allow-other-keys))
(defgeneric send-request (client req &key &allow-other-keys))
(defgeneric receive-response (service stream &key))
(defgeneric receive-request (client res &key))

(defgeneric response-ok-p (res)
  (:method ((res response)) t))

(defgeneric response-status (res))

(defgeneric (setf response-status) (new res))

(defverb accept (self))

;;; Config
(defconfig service-config (id ast) 
  ((request-class :initarg :request-class)
   (response-class :initarg :response-class))
  (:default-initargs
   :request-class 'request
   :response-class 'response))

(defmethod service ((self service-config))
  "Try to find a suitable SERVICE class matching the class-name of a SERVICE-CONFIG."
  (let ((name (remove-string "-CONFIG" (string (class-name (class-of self))))))
    (find-class (symbolicate name))))

(defmethod load-ast ((self service-config))
  (with-slots (ast) self
    (doplist (k v) ast
      (setf (slot-value self (find-symbol (string k))) v))
    (setf ast nil)
    self))

(defmethod build ((self service-config) &key (nullp nil) (exclude '(ast id)))
  (setf (ast self)
        (unwrap-object self
                       :slots t
                       :methods nil
                       :nullp nullp
                       :exclude exclude)))

(defun build-service (self)
  (make-instance (service self)
    :request-class (slot-value self 'request-class)
    :response-class (slot-value self 'response-class)))

(defmethod make-config ((self (eql :service)) &rest args &key (class 'service-config))
  (apply 'make-instance class (remove-from-plist args class)))

(defmacro defservice (name super slots &rest opts)
  "Define a SERVICE subclass."
  `(defclass ,name ,(or super '(service)) ,slots ,@opts))

(defmacro with-service ((name &key pool) &body body)
  "Bind *SERVICE* around BODY.

When POOL is non-nil it should be a form passed to WITH-THREAD-POOL."
  `(let ((*service* (gethash ,name *service-table*)))
     ,@(if pool
           `((with-thread-pool ,pool ,@body))
           body)))
