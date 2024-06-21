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

;;; Code:
(in-package :net/srv)

;;; Vars
(defvar *router*)
(defvar *acceptor*)
(defvar *handlers*)

;;; Conditions
;; from hunchentoot
(define-condition srv-condition (condition) ())
(deferror srv-error (srv-condition error) () (:auto t))
(deferror srv-simple-error (srv-error simple-condition) () (:auto t))

(define-condition srv-warning (srv-condition warning) ())
(define-condition srv-simple-warning-warning (srv-warning simple-condition) ())

(deferror bad-request (srv-error))

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

;;; Router

;;; Service
(defclass service (obj/id:id)
  ((address)
   (request-class)
   (response-class)
   (task-pool)
   (read-timeout)
   (write-timeout)
   (home)
   (connection-max)
   (chunk-output-p)
   (chunk-input-p)
   (socket)
   (request-count :initform 0)
   (shutdown-lock :initform (sb-thread:make-mutex :name "shutdown-lock"))
   (shutdown-queue :initform (sb-thread:make-waitqueue :name "shutdown-queue")))
  (:default-initargs
   :id (gensym "SRV")))

;;; Macros
(defmacro define-service (name &rest initargs)
  "Define a subclass of NET/SRV:SERVICE."
  `(defclass ,name ,@initargs))
