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

;;; Conditions
;; from hunchentoot
(define-condition srv-error () ())

(define-condition srv-simple-error (srv-error simple-condition) ())

(defun srv-simple-error (format-control &rest format-arguments)
  (error 'srv-simple-error
         :format-control format-control
         :format-arguments format-arguments))

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

;;; Macros
(defmacro define-service (name &rest initargs)
  "Define a subclass of NET/SRV:SERVICE."
  `(defclass ,name ,@initargs))
