;;; cli.lisp --- cli programming api and utils

;; This package contains a simple api and macros for building lisp CLI
;; programs.

;;; Commentary:

;; - inspired by: clingon, uiop

;; Basic assumptions at runtime:
;;   - running in a POSIX-compliant shell
;;   - output stream supports UTF-8

;; TODO 2023-10-14: install-ast, install-thunk, proc-args, etc should
;; return IR types - CLI-IR THUNK and CLI-IR respectively.

;; TODO 2023-10-14: rename cli-ast to cli-ir, install-ast to
;; install-ir, etc.

;;; Code:
(in-package :std-user)
(defpkg :cli/int 
  (:use :cl :std) 
  (:export :*cli-packages* :*cli-tool-packages* :*cli-clap-packages*))

(in-package :cli/int)

(defparameter *cli-packages* nil)

(setq *defpkg-hook* (lambda (x) (pushnew (package-name x) *cli-packages* :test 'string=)))

(defpkg :cli/shell
  (:use :cl :std)
  (:nicknames :shell)
  (:export :*shell* :*shell-directory* :*shell-input*))

(defpkg :cli/env
  (:use :cl :std)
  (:export :*default-global-env-var-names* :*default-local-env-var-names* :exec-path-list
   :program-list :find-exe :ld-library-path-list :concat-env-table
   :make-env-var
   :load-env
   :*env-table*))

(defpkg :cli/ansi
  (:use :cl :std)
  (:nicknames :ansi)
  (:export
   ;; ESC sequences
   :.ris :reset-to-initial-state
   ;; CSI sequences | cursor control
   :.cuu :cursor-up
   :.cud :cursor-down
   :.cuf :cursor-forward
   :.cub :cursor-backward
   :.cnl :cursor-next-line
   :.cpl :cursor-preceding-line
   :.cha :cursor-horizontal-absolute
   :.cup :cursor-position
   :.vpa :vertical-position-absolute
   :.vpr :vertical-position-relative
   :.vpb :vertical-position-backward
   :.scosc :save-cursor-position
   :.scorc :restore-cursor-position
   :.ed :erase-in-display :erase-below :erase-above :erase :erase-saved-lines
   :.el :erase-in-line :erase-right :erase-left :erase-line
   :.sgr :select-graphic-rendition
   :.dsr :device-status-report
   ;; DEC private mode set and reset
   :.decset :dec-private-mode-set
   :.decrst :dec-private-mode-reset
   :show-cursor :hide-cursor
   :use-alternate-screen-buffer :use-normal-screen-buffer
   ;; common
   :clear
   :home
   ;; stty
   :set-tty-mode))

(defpkg :cli/prompt
  (:use :cl :std :obj/equiv)
  (:export
   :completing-read
   :defprompt))
   
(defpkg :cli/progress
  (:use :cl :std)
  (:export
   :update-progress
   :with-progress-bar
   :*progress-bar*
   :*progress-bar-enabled*
   :start-progress-display
   :finish-progress-display
   :progress-mutex
   :uncertain-size-progress-bar
   :progress-bar
   :with-progress-maybe))

(defpkg :cli/spark
  (:use :cl :std)
  (:export
   :spark :*ticks*
   :vspark :*vticks*))

(defpkg :cli/repl
  (:use :cl :std :cli/progress :cli/spark)
  (:export :load-acl-repl :start-rl-repl
           :default-toplevel-init))

(defpkg :cli/ed
  (:use :cl :std :cli/env :ast :config)
  (:export :run-emacs :run-emacsclient :org-store-link :editor-config :emacs-config
   :eval-emacs
   :slime
   :ielm
   :with-emacs
   :emacs-find-file))

(defpkg :cli/multi
  (:use :cl :std :clap)
  (:export
   #:define-multi-main
   #:make-symlinks))

(defpkg :cli/tui
  (:use :cl :std :ansi)
  (:export))

(setq *defpkg-hook* nil)
 
