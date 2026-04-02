;;; pam.lisp --- Linux PAM

;; 

;;; Code:
(in-package :security)

(define-opaque pam-handle)

(define-alien-type pam-message
  (struct pam-message
    (msg-style int)
    (msg c-string)))

(define-alien-type pam-response
    (struct pam-response
      (resp c-string)
      (resp-retcode int)))

(define-alien-type pam-conv
    (struct pam-conv
      (conv (* (function int int (* (* pam-message)) (* (* pam-response)) (* t))))
      (appdata-ptr (* t))))

(define-alien-type pam-xauth-data
    (struct pam-xauth-data
      (namelen int)
      (name c-string)
      (datalen int)
      (data c-string)))

(defar pam-start int
  (service-name c-string)
  (user c-string)
  (pam-conversation (* pam-conv))
  (pamh (* (* pam-handle))))

(defar pam-start-confdir int
  (service-name c-string)
  (user c-string)
  (pam-conversation (* pam-conv))
  (confdir c-string)
  (pamh (* (* pam-handle))))

(defar pam-end int
  (pamh (* pam-handle))
  (pam-status int))

(defar pam-authenticate int
  (pamh (* pam-handle))
  (flags int))

(defar pam-setcred int
  (pamh (* pam-handle))
  (flags int))

(defar pam-acct-mgmt int
  (pamh (* pam-handle))
  (flags int))

(defar pam-open-session int
  (pamh (* pam-handle))
  (flags int))

(defar pam-close-session int
  (pamh (* pam-handle))
  (flags int))

(defar pam-chauthtok int
  (pamh (* pam-handle))
  (flags int))
