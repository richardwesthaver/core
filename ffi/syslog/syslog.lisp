;;; ffi.lisp --- Syslog FFI

;; 

;;; Code:
(in-package :syslog)

(defar closelog void)
(defar openlog void (ident c-string) (option int) (facility int))
(defar setlogmask int (mask int))
(defar syslog void (pri int) (fmt c-string))
