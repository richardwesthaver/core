#!/bin/core --script
(in-package :user)
(make-thread-pool (num-cpus) :name :emacs)

(defun emacs-server (name)
  (lambda () (cli:run-emacs nil :wait nil :output t :server name)))

(defmacro emacs-send (name &body body)
  (with-gensyms (e)
    `(cli:with-emacs (,e :server ,name :output t)
       ,@body)))

(defvar *emacs-servers* nil)

;; start 2 (background) emacs daemons
(with-thread-pool (:emacs)
  (with-channel (ch 2)
    (submit-work ch (emacs-server "publish"))
    (submit-work ch (emacs-server "skel"))
    (push (receive-result ch) *emacs-servers*)
    (push (receive-result ch) *emacs-servers*)))

;; schedule publish timers
