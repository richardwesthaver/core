;;; pam.lisp --- Linux PAM

;; 

;;; Code:
(in-package :security)

(define-alien-loader :pam)

(define-alien-enum (pam-result)
  :success pam-success
  :open-err pam-open-err
  :symbol-err pam-symbol-err
  :service-err pam-service-err
  :system-err pam-system-err
  :buf-err pam-buf-err
  :perm-denied pam-perm-denied
  :auth-err pam-auth-err
  :cred-insufficient pam-cred-insufficient
  :authinfo-unavail pam-authinfo-unavail
  :user-unknown pam-user-unknown
  :maxtries pam-maxtries
  :new-authtok-reqd pam-new-authtok-reqd
  :acct-expired pam-acct-expired
  :session-err pam-session-err
  :cred-unavail pam-cred-unavail
  :cred-expired pam-cred-expired
  :cred-err pam-cred-err
  :no-module-data pam-no-module-data
  :conv-err pam-conv-err
  :authtok-err pam-authtok-err
  :authtok-recovery-err pam-authtok-recovery-err
  :authtok-lock-busy pam-authtok-lock-busy
  :authtok-disable-aging pam-authtok-disable-aging
  :try-again pam-try-again
  :ignore pam-ignore
  :abort pam-abort
  :authtok-expired pam-authtok-expired
  :module-unknown pam-module-unknown
  :bad-item pam-bad-item
  :conv-again pam-conv-again
  :incomplete pam-incomplete
  :return-values pam-return-values)

(define-alien-enum (pam-flag)
  :silent pam-silent
  :disallow-null-authtok pam-disallow-null-authtok
  :establish-cred pam-establish-cred
  :delete-cred pam-delete-cred
  :reinitialize-cred pam-reinitialize-cred
  :refresh-cred pam-refresh-cred
  :change-expired-authtok pam-change-expired-authtok)

(define-alien-enum (pam-item-type)
  :service pam-service
  :user pam-user
  :tty pam-tty
  :rhost pam-rhost
  :conv pam-conv
  :authtok pam-authtok
  :oldauthtok pam-oldauthtok
  :ruser pam-ruser
  :user-prompt pam-user-prompt
  :fail-delay pam-fail-delay
  :xdisplay pam-xdisplay
  :xauthdata pam-xauthdata
  :authtok-type pam-authtok-type)

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

(defar pam-start pam-result
  (service-name c-string)
  (user c-string)
  (pam-conversation (* pam-conv))
  (pamh (* (* pam-handle))))

(defar pam-start-confdir pam-result
  (service-name c-string)
  (user c-string)
  (pam-conversation (* pam-conv))
  (confdir c-string)
  (pamh (* (* pam-handle))))

(defar pam-end pam-result
  (pamh (* pam-handle))
  (pam-status pam-result))

(defar pam-authenticate pam-result
  (pamh (* pam-handle))
  (flags int))

(defar pam-setcred pam-result
  (pamh (* pam-handle))
  (flags int))

(defar pam-acct-mgmt pam-result
  (pamh (* pam-handle))
  (flags int))

(defar pam-open-session pam-result
  (pamh (* pam-handle))
  (flags int))

(defar pam-close-session pam-result
  (pamh (* pam-handle))
  (flags int))

(defar pam-chauthtok pam-result
  (pamh (* pam-handle))
  (flags int))

;;; Utils
(defun pam-flags (&rest flags)
  (apply 'logior (mapcar 'pam-flag flags)))

(defmacro with-pam ((sym conv err name &optional (user (current-user))) &body body)
  "SYM CONV STAT are bound over BODY, NAME is service name, USER current user"
  ;; heap allocate so that PAM may free
  `(let ((,sym (make-alien (* pam-handle)))
         (,conv (make-alien pam-conv))
         (,err))
     (setf ,err (pam-start ,name ,user ,conv ,sym))
     ,@body
     (values 
      ,err
      (pam-end (deref ,sym) ,err))))
