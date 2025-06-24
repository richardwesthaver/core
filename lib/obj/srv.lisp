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

;; request/response? here
;; engine? either here or obj/eng.lisp, build on std/task, std/thread

;; %service-protocol

;; session? prob here or in HTTP/S impl
;; connection? lower-level than session

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
(defvar *service-table* (make-hash-table :weakness :value))
(defvar *request* nil)
(defvar *response* nil)

;;; Utils
(defun in-request-p () (and (boundp '*request*) *request*))
(defun in-response-p () (and (boundp '*response*) *response*))

;;; Conditions
(define-condition service-condition (condition) ())
(eval-always
  (deferror service-error (service-condition error) () (:auto t)))
(deferror simple-service-error (service-error simple-condition) () (:auto t))

(define-condition service-warning (service-condition warning) ())

(defwarning simple-service-warning (service-warning simple-warning) () (:auto t))

(deferror bad-request (service-error) ())

;;; Objects
(defclass engine () 
  ((service :accessor service :initarg :service 
            :documentation "A link to the SERVICE which owns this instance.")))

(defclass service (id)
  ((request-class :type symbol :initarg :request-class :accessor service-request-class)
   (response-class :type symbol :initarg :response-class :accessor service-response-class)
   (engine :type engine :accessor engine :initarg :engine)))

(defclass response () ())

(defgeneric make-response (&rest args &key &allow-other-keys))

(defclass service-response (response)
  ((content-type :reader content-type)
   (content-length :reader content-length :initform nil)))

(defmethod response-ok-p ((res response)) t)

(defclass request ()
  ((data :initarg :data :accessor data)))

(defgeneric make-request (&rest args &key &allow-other-keys))

(defclass service-request (request)
  ((content-stream :initarg :content-stream :reader content-stream)
   (service :initarg :service
	    :reader service)
   (session :initform nil
	    :accessor session)
   (protocol :initarg :request-protocol :reader request-protocol)))

(defconfig service-config () ())
  
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

(defgeneric process-request (req)
  (:documentation "Function called by PROCESS-CONNECTION after reading incoming headers. Calls
HANDLE-REQUEST to dispatch to a route and return output to the client using
START-OUTPUT.

Return value is ignored."))

(defgeneric handle-request (self request)
  (:documentation "Function called after fetching a request. Used to establish error handling,
logging, etc."))
(defgeneric dispatch-request (self request)
  (:documentation "Function called after 'handle-request' which routes a request to a service."))
(defgeneric send-response (service stream &key content &allow-other-keys))

(defgeneric response-ok-p (res))
(defgeneric response-status (res))
(defgeneric (setf response-status) (new res))

;;; Tasks
(define-task-kernel service-task-kernel () ()
  "Default task kernel for service-based tasks.")
