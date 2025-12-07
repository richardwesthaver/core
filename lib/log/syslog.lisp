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

 ;; priorities/facilities are encoded into a single 32-bit quantity, where the
 ;; bottom 3 bits are the priority (0-7) and the top 28 bits are the facility
 ;; (0-big number).  Both the priorities and the facilities map roughly
 ;; one-to-one to strings in the syslogd(8) source code.  This mapping is
 ;; included in this file.

;; priorities (these are ordered)
(define-alien-enum (syslog-priority :type (sb-alien:unsigned 8))
  :EMERG 0 ; system is unusable 
  :ALERT 1 ; action must be taken immediately 
  :CRIT	2 ; critical conditions 
  :ERR 3 ; error conditions 
  :WARNING 4 ; warning conditions 
  :NOTICE 5 ; normal but significant condition 
  :INFO 6 ; informational 
  :DEBUG 7) ; debug-level messages 

;; facility codes
(define-alien-enum (syslog-facility :type (sb-alien:unsigned 8))
  :KERN (ash 0 3) ; kernel messages
  :USER (ash 1 3) ; random user-level messages 
  :MAIL (ash 2 3) ; mail system 
  :DAEMON (ash 3 3) ; system daemons 
  :AUTH (ash 4 3) ; security/authorization messages 
  :SYSLOG (ash 5 3) ; messages generated internally by syslogd 
  :LPR (ash 6 3) ; line printer subsystem 
  :NEWS (ash 7 3) ; network news subsystem 
  :UUCP (ash 8 3) ; UUCP subsystem 
  :CRON (ash 9 3) ; clock daemon 
  :AUTHPRIV (ash 10 3) ; security/authorization messages (private) 
  :FTP (ash 11 3) ; ftp daemon 
  ;; other codes through 15 reserved for system use
  :LOCAL0 (ash 16 3) ; reserved for local use 
  :LOCAL1 (ash 17 3) ; reserved for local use 
  :LOCAL2 (ash 18 3) ; reserved for local use 
  :LOCAL3 (ash 19 3) ; reserved for local use 
  :LOCAL4 (ash 20 3) ; reserved for local use 
  :LOCAL5 (ash 21 3) ; reserved for local use 
  :LOCAL6 (ash 22 3) ; reserved for local use 
  :LOCAL7 (ash 23 3)) ; reserved for local use 

;; openlog syslog closelog
(defun syslog-socket-p (&optional (path "/dev/log"))
  (sb-posix:s-issock (sb-posix::stat-mode (sb-posix:stat path))))
