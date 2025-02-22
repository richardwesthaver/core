;;; whois.lisp --- WHOIS Protocol Support

;; 

;;; Commentary:

;; ref: https://datatracker.ietf.org/doc/html/rfc3912

;;; Code:
(in-package :net/proto/whois)

(eval-when (:load-toplevel)
  (pushnew :rfc3912 *features*))
