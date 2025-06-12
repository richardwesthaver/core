;;; net/tcp.lisp --- Network Objects

;;

;;; Code:
(in-package :net/core)

;;; Types
(deftype port () "Port number" '(integer 0 65535))
(deftype unprivileged-port () "Unprivileged port number" '(or (port 1024 65535) (port 0)))
(deftype privileged-port () "Privileged port number" '(port 1 1023))
(deftype ip-address () "IP Address specifier" '(or string (vector unsigned-byte) list))
(deftype socket-address () "A complete internet socket address specifier." '(cons ip-address port))

;;; Conditions
(define-condition net-condition () ())
(define-condition codec-condition (net-condition) ())
(define-condition protocol-condition (net-condition) ())

(define-condition net-error (net-condition std-error) ())
(define-condition codec-warning (codec-condition std-warning) ())
(define-condition protocol-warning (protocol-condition std-warning) ())
(define-condition codec-error (codec-condition net-error) ())
(define-condition protocol-error (protocol-condition net-error) ())

;;; Classes
(defconfig net-config () 
  ())

(defclass transport ()
  ())

(defclass connection () ())

(defclass codec ()
  ())

(defclass protocol ()
  ())

(defclass client (sb-bsd-sockets:inet-socket) ())

(defconfig client-config (net-config) ())

(defclass server (sb-bsd-sockets:inet-socket) ())
(defconfig server-config (net-config) ())

(defclass peer () ())

(defclass proxy (client server) ())

(defclass tunnel (client server) ())  

;;; Protocol
(defgeneric connect (self &key &allow-other-keys))
(defgeneric disconnect (self &key &allow-other-keys))
(defgeneric make-client (kind &rest initargs &key &allow-other-keys))
(defgeneric make-server (kind &rest initargs &key &allow-other-keys))
