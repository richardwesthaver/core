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
                                 &more ...)
 (with-ws (ws 'my-homepage)
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
(defvar *global-session-db* nil)
(defvar *global-session-db-lock* (make-mutex :name "global-session-db"))
(defvar *default-connection-timeout* 20)
(defvar *default-service-port* 8000)
#+ssl (defvar *default-ssl-service-port* 8000)
(defvar *default-session-timeout* #.(* 30 60)) ;; 30m
(defvar *default-content-type* "text/html")
;;; Conditions
;; from hunchentoot
(define-condition srv-condition (condition) ())
(deferror srv-error (srv-condition error) () (:auto t))
(deferror srv-simple-error (srv-error simple-condition) () (:auto t))

(define-condition srv-warning (srv-condition warning) ())
(define-condition srv-simple-warning (srv-warning simple-condition) ())

(deferror bad-request (srv-error) ())

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
(defgeneric start-service (self)
  (:documentation "Start a service."))

(defgeneric stop-service (self)
  (:documentation "Stop a service."))

(defgeneric restart-service (self)
  (:documentation "Restart a service.")
  (:method ((self t))
    (stop-service self)
    (start-service self)))

(defgeneric add-route (self uri handler &key &allow-other-keys))
(defgeneric delete-route (self uri &key &allow-other-keys))
(defgeneric handle-request (self request))
(defgeneric service-name (self)
  (:method ((self t))
    (obj/id:id self)))

;;; Response
(defclass service-response () ())
(defclass http-service-response (service-response) ((response :type http-response)))

;;; Request
(defclass service-request ()
  ((origin :initarg :origin
           :reader request-origin)
   (session :initform nil
            :accessor session)))

(defclass http-service-request (service-request)
  ((request :type http-request)))

;;; Session

;; HACK 2024-07-18: currently not storing the SESSION-STRING directly in this
;; class as a slot. may need to change but I would rather have the string
;; cached/displaced to some other location.. depends how often we need that
;; string.
(defclass session (obj/id:id)
  ((id :type integer)
   (user-agent)
   (remote-addr)
   (session-start)
   (last-click)
   (data)
   (max-time :type fixnum))
  (:default-initargs
   :session-start (get-universal-time)
   :last-click (get-universal-time)
   :max-time *default-session-timeout*))

;;; Headers

;;; Router
(defclass router () ())

;;; Task Pool
;; Automatic Multithreading support for service objects

(define-task-kernel service-task-kernel () ()
  "Default task kernel for service-based tasks.")

;; supervisor, worker, task, kernel
;;; Service
(defclass service (obj/id:id)
  ((port)
   (address)
   ;; HACK 2024-07-18: will this only accept class names? are structures classes? (http-request, http-response)
   (request-class :type symbol)
   (response-class :type symbol)
   (task-pool :type task-pool)
   (read-timeout :type fixnum)
   (write-timeout :type fixnum)
   (connection-max :type fixnum)
   ;; RESEARCH 2024-07-18: 
   ;; may need to start dealing with this
   ;; https://datatracker.ietf.org/doc/html/rfc2616#section-3.6.1
   (chunk-output-p :type boolean)
   (chunk-input-p :type boolean)
   (socket :type socket)
   (request-count :type integer)
   (shutdown-lock :type mutex)
   (shutdown-queue :type waitqueue))
  (:default-initargs
   :id (symbol-name (gensym "srv"))
   :port *default-service-port*
   :address nil
   :request-class 'service-request
   :response-class 'service-response
   :chunk-output-p t
   :chunk-input-p t
   :read-timeout *default-connection-timeout*
   :write-timeout *default-connection-timeout*
   :request-count 0
   :shutdown-lock (sb-thread:make-mutex :name "shutdown-lock")
   :shutdown-queue (sb-thread:make-waitqueue :name "shutdown-queue"))
  (:documentation "The service class is designed primarily for webservers and functionally
similar to HUNCHENTOOT:ACCEPTOR."))

#+ssl
(defclass ssl-service (service)
  ((certificate-file :initarg :certificate-file
                     :reader service-certificate-file)
   (privatekey-file :initarg :privatekey-file
                    :reader service-privatekey-file)
   (privatekey-password :initarg :privatekey-password
                        :reader service-privatekey-password))
  (:default-initargs
   :password nil
   :port 443))

;;; Macros
(defmacro define-service (name &rest initargs)
  "Define a subclass of NET/SRV:SERVICE."
  `(defclass ,name ,@initargs))
