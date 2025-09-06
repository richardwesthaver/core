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
(defclass sys-logger (logger) ())
