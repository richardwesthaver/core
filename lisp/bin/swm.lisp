;;; swm.lisp --- Core StumpWM Launcher

;; 

;;; Code:
(defpackage :bin/swm
  (:use :cl :std :log :gui :cli)
  (:export
   #:start-swm
   #:*swm-cli*))

(in-package :bin/swm)

(define-cli *swm-cli*
  :name "swm"
  :help t
  :version 0
  :thunk stumpwm:stumpwm)

(defmain start-swm ()
  (with-cli (*swm-cli* :args (cli:args))
    (in-package :stumpwm-user)
    (cli:do-cmd cli:*cli*)))
