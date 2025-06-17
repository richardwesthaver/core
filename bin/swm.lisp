;;; swm.lisp --- Core StumpWM Launcher

;; 

;;; Code:
(defpackage :bin/swm
  (:use :cl :std :log :gui :cli :clap)
  (:export
   #:start-swm
   #:*swm-cli*))

(in-package :bin/swm)

(defcmd stumpwm-cmd ()
  (stumpwm:stumpwm (or (car *args*) (sb-posix:getenv "DISPLAY") ":0")))

(define-cli *swm-cli*
  :name "swm"
  :help t
  :version 0
  :thunk stumpwm-cmd
  :cmds ((:name "start" :thunk stumpwm-cmd :description "Start StumpWM")))

(defmain start-swm ()
  (with-cli (*swm-cli* :args nil)
    (do-cmd *cli*)))
