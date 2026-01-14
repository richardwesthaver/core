;;; sys.lisp --- System Log

;; System logging facilities

;;; Commentary:

;; A SYS-LOGGER is used for system-level logging, usually by sending messages
;; out to an OS service like Journald or syslog.

;;; Code:
(in-package :log)

;;; Journald interface

;;; Objects
(defclass journal-sink (sink) ())
(defclass syslog-sink (sink) ())

;; openlog syslog closelog
(defun syslog-socket-p (&optional (path "/dev/log"))
  (sb-posix:s-issock (sb-posix::stat-mode (sb-posix:stat path))))

(defclass sys-logger (logger) 
  ((id :initarg :id :initform (file-namestring (car sb-ext:*posix-argv*)) :accessor id)
   (options :accessor syslog-options)
   (facility :accessor syslog-facility))
  (:documentation "A LOGGER which outputs to a system log."))

(defmethod start :before ((self sys-logger))
  (with-slots (options id facility) self
    (syslog:openlog
     id
     (reduce (lambda (x y) (logand (syslog-options x) (syslog-options y))) options)
     (syslog-facility facility))))

(defmethod stop :after ((self sys-logger) &key)
  (syslog:closelog))
