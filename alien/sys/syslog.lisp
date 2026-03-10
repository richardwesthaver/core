;;; ffi.lisp --- Syslog FFI

;; 

;;; Code:
(in-package :sys)

(define-alien-enum (syslog-option)
  :cons +log-cons+
  :ndelay +log-ndelay+
  :nowait +log-nowait+
  :odelay +log-odelay+
  :perror +log-perror+
  :pid +log-pid+)

;; priorities/facilities are encoded into a single 32-bit quantity, where the
;; bottom 3 bits are the priority (0-7) and the top 28 bits are the facility
;; (0-big number).  Both the priorities and the facilities map roughly
;; one-to-one to strings in the syslogd(8) source code.  This mapping is
;; included in this file.

;; priorities (these are ordered)
(define-alien-enum (syslog-priority :type (sb-alien:unsigned 8))
  :EMERG +log-emerg+ ; system is unusable 
  :ALERT +log-alert+ ; action must be taken immediately 
  :CRIT	+log-crit+ ; critical conditions 
  :ERR +log-err+ ; error conditions 
  :WARNING +log-warning+ ; warning conditions 
  :NOTICE +log-notice+ ; normal but significant condition 
  :INFO +log-info+ ; informational 
  :DEBUG +log-debug+) ; debug-level messages 

;; facility codes
(define-alien-enum (syslog-facility :type (sb-alien:unsigned 8))
  :KERN +log-kern+ ; kernel messages
  :USER +log-user+ ; random user-level messages 
  :MAIL +log-mail+ ; mail system 
  :DAEMON +log-daemon+ ; system daemons 
  :AUTH +log-auth+ ; security/authorization messages 
  :SYSLOG +log-syslog+ ; messages generated internally by syslogd 
  :LPR +log-lpr+ ; line printer subsystem 
  :NEWS +log-news+ ; network news subsystem 
  :UUCP +log-uucp+ ; UUCP subsystem 
  :CRON +log-cron+ ; clock daemon 
  :AUTHPRIV +log-authpriv+ ; security/authorization messages (private) 
  :FTP +log-ftp+ ; ftp daemon 
  ;; other codes through 15 reserved for system use
  :LOCAL0 +log-local0+ ; reserved for local use 
  :LOCAL1 +log-local1+ ; reserved for local use 
  :LOCAL2 +log-local2+ ; reserved for local use 
  :LOCAL3 +log-local3+ ; reserved for local use 
  :LOCAL4 +log-local4+ ; reserved for local use 
  :LOCAL5 +log-local5+ ; reserved for local use 
  :LOCAL6 +log-local6+ ; reserved for local use 
  :LOCAL7 +log-local7+) ; reserved for local use 

(defar closelog void)
(defar openlog void (ident c-string) (option int) (facility int))
(defar setlogmask int (mask int))
(defar syslog void (pri int) (fmt c-string))
