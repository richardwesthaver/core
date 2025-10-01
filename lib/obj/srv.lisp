;;; srv.lisp --- Sans-IO Service Protocol

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

;; Tower: https://github.com/tower-rs/tower

;; Axum: https://github.com/tokio-rs/axum

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

(std:definline register-service (name srv)
  (setf (gethash name *service-table*) srv))

;;; Conditions
(define-condition service-condition (condition) ())
(eval-always
  (deferror service-error (service-condition error) () (:auto t)))
(deferror simple-service-error (service-error simple-condition) () (:auto t))

(define-condition service-warning (service-condition warning) ())

(defwarning simple-service-warning (service-warning simple-warning) () (:auto t))

(deferror bad-request (service-error) ())

;;; Objects
(defclass service (id)
  ((request-class :type symbol :initarg :request-class :accessor service-request-class)
   (response-class :type symbol :initarg :response-class :accessor service-response-class)
   (engine :type engine :accessor engine :initarg :engine))
  (:documentation "Base Class shared by all services. A service must specify the request and
response classes it uses for communication as well as the engine which drives it."))

(defclass engine ()
  ((service :accessor service :initarg :service 
            :documentation "A link to the SERVICE which owns this instance."))
  (:documentation "An engine provides an execution context for a SERVICE. Engines are responsible
for managing the work done by a service and distributing work to compute
resources. Different engines may use the main thread for execution, a
dedicated thread, their own THREAD-POOL, or a combination of threading
strategies."))

(defclass response () ()
  (:documentation "Base class for response objects, usually generated in reply to a REQUEST."))

(defclass service-response (response)
  ((content-type :reader content-type)
   (content-length :reader content-length :initform nil)))

(defmethod response-ok-p ((res response)) t)

(defclass request ()
  ((data :initarg :data :accessor data))
  (:documentation "Base class for request objects, often paired with RESPONSE objects."))

(defclass service-request (request)
  ((content-stream :initarg :content-stream :reader content-stream)
   (service :initarg :service
	    :reader service)
   (session :initform nil
	    :accessor session)
   (protocol :initarg :request-protocol :reader request-protocol)))

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

(defgeneric response-ok-p (res))

(defgeneric response-status (res))

(defgeneric (setf response-status) (new res))

;;; Config
(defconfig service-config (id:id ast:ast) 
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
      (unless (null v) (setf (slot-value self (symbolicate k)) v)))
    (setf ast nil)
    self))

(defmethod build-ast ((self service-config) &key (nullp nil) (exclude '(ast id)))
  (setf (ast self)
        (unwrap-object self
                       :slots t
                       :methods nil
                       :nullp nullp
                       :exclude exclude)))

(defmethod build ((self service-config) &key)
  (make-instance (service self)
    :request-class (slot-value self 'request-class)
    :response-class (slot-value self 'response-class)))

(defmethod make-config ((self (eql :service)) &rest args &key (class 'service-config))
  (apply 'make-instance class (remove-from-plist args class)))
