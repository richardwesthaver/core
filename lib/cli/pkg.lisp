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

(setq *defpkg-hook* (compile nil (lambda (x) (pushnew (package-name x) *cli-packages* :test 'string=))))

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
   :.ris
   ;; CSI sequences | cursor control
   :.cuu
   :.cud
   :.cuf
   :.cub
   :.cnl
   :.cpl
   :.cha
   :.cup
   :.vpa
   :.vpr
   :.vpb
   :.scosc
   :.scorc
   :.ed :erase-below :erase-above :erase :erase-saved-lines
   :.el :erase-right :erase-left :erase-line
   :.sgr
   :.dsr
   ;; DEC private mode set and reset
   :.decset
   :.decrst
   :show-cursor :hide-cursor
   :use-alternate-screen-buffer :use-normal-screen-buffer
   ;; common
   :clear
   :home
   ;; stty
   :set-tty-mode))

(defpkg :cli/terminfo
  (:nicknames :ti :terminfo)
  (:use :cl)
  (:shadow :columns)
  (:import-from :std :winsize :+tiocgwinsz+)
  (:export
   :*terminfo-directories* :*terminfo*
   :capability :tparm :tputs :decode-padding
   :set-terminal :capabilities))

(defpkg :cli/linedit
  (:nicknames :linedit)
  (:use :cl :std)
  (:import-from :sb-posix :getenv :ioctl :tcgetattr :tcsetattr :termios)
  (:import-from :terminfo :tputs :set-terminal :tparm)
  (:import-from :std
   :with-gensyms :with-directory-iterator
   :file-kind :current-directory
   :relative-pathname-p :if-let
   #:isatty #:winsize)
  (:export
   #:linedit
   #:formedit
   #:*default-columns*
   #:*default-lines*
   #:*highlight-color*
   #:install-repl
   #:uninstall-repl
   #:*announce*))

(defpkg :cli/prompt
  (:use :cl :std :obj/equiv :linedit)
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
   :ediff
   :ediff3
   :vc-ediff
   :with-emacs
   :emacs-find-file))

(defpkg :cli/multi
  (:use :cl :std)
  (:export
   #:define-multi-main
   #:make-symlinks))

(defpkg :cli/tui
  (:use :cl :std :ansi)
  (:export))

(setq *defpkg-hook* nil)
