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
(defpkg :cli/clap
  (:nicknames :clap)
  (:use :cl :std :log :cmd :ast :equiv)
  (:use-reexport :log :cmd :ast)
  (:export :*no-exit*
   :*default-cli-class* :*cli*
   :*cli-table* :*no-debug*
   :schar0 :with-cli-handlers 
   :cli-command :cli
   :print-version
   :make-cli :define-cli
   :cli-cd :with-cli
   :load-cli))

(defpkg :cli/shell
  (:use :cl :std)
  (:nicknames :shell)
  (:export :*shell* :*shell-input*))

(defpkg :cli/env
  (:use :cl :std)
  (:nicknames :env)
  (:export :*default-global-env-var-names* :*default-local-env-var-names* :exec-path-list
   :program-list :find-exe :ld-library-path-list :concat-env-table
   :make-env-var
   :load-env
   :get-env
   :*env-table*))

(defpkg :cli/ansi
  (:use :cl :std :color)
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

(defpkg :cli/term
  (:nicknames :ti :terminfo :term)
  (:use :cl :config :std)
  (:shadow :columns)
  (:import-from :std :winsize :+tiocgwinsz+)
  (:export
   :*terminfo-directories* :*terminfo*
   :capability :tparm :tputs :decode-padding
   :set-terminal :capabilities
   :cursor-left :cursor-right
   :cursor-up :cursor-down
   :column-address :auto-right-margin
   :clr-eos :enter-am-mode
   :set-a-foreground :enter-bold-mode
   :exit-attribute-mode :dumb-terminal-p))

(defpkg :cli/ed
  (:use :cl :std :cli/env :ast :config :cmd)
  (:export 
   :run-emacs :run-emacsclient :org-store-link :editor-config 
   :emacs-config :*user-emacs-directory*
   :*user-org-directory*
   :*editor-config*
   :eval-emacs
   :with-editor-point-and-string
   :editor-insert-mode
   :move-to-eol
   :move-to-bol
   :slime
   :ielm
   :ediff
   :ediff3
   :vc-ediff
   :with-emacs
   :emacs-find-file
   :editor
   :line
   :rewindable
   :get-line
   :get-string
   :get-point
   :text-buffer
   :copy-buffer
   :ensure-buffer
   :buffer-push
   :buffer-find-next-if
   :buffer-find-previous-if
   :buffer-previous
   :buffer-peek
   :buffer-next
   :buffer-cycle
   :save-state
   :rewind-state
   #:edit-file
   #:*editor*
   :quoted-p
   :find-open-quote
   :find-close-quote
   :find-open-paren
   :find-close-paren
   :dwim-mark-parens
   :editor-word-start
   :editor-previous-word-start
   :editor-next-word-end
   :editor-word
   :editor-sexp-start
   :editor-sexp-end
   :editor-command
   :editor-replace-word
   :in-quoted-string-p))

(defpkg :cli/progress
  (:use :cl :std)
  (:nicknames :progress :pbar)
  (:export
   :update-progress
   :update-progress-display
   :update!
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
  (:nicknames :spark)
  (:export
   :spark :*ticks*
   :vspark :*vticks*))

(defpkg :cli/repl
  (:use :cl :std :cli/progress :cli/spark :config :ast :color :cli/term)
  (:export :load-acl-repl :start-rl-repl :make-toplevel-init))

(defpkg :cli/linedit
  (:nicknames :linedit)
  (:use :cl :std :cli/term :kbd :cmd :cli/ed)
  (:shadowing-import-from :sb-posix :ioctl)
  (:import-from :sb-posix :getenv :tcgetattr :tcsetattr :termios)
  (:import-from :terminfo :tputs :set-terminal :tparm)
  (:import-from :std
   :with-gensyms :with-directory-iterator
   :file-kind :current-directory
   :relative-pathname-p :if-let
   #:isatty #:winsize)
  (:export
   #:linedit
   #:yes-or-no
   #:formedit
   #:*default-columns*
   #:*default-lines*
   #:*highlight-color*
   #:install-repl
   #:uninstall-repl
   #:make-editor
   #:*announce*))

(defpkg :cli/main
  (:use :cl :std :cmd)
  (:import-from :cli/clap :*no-exit* :*no-debug* :with-cli-handlers)
  (:export
   #:defmain
   #:define-multi-main))

(defpkg :cli/tui
  (:use :cl :std :ansi :linedit :progress :spark :terminfo :env :cmd :clap :secret)
  (:export :completing-read :completing-read-form))

(defvar *cli-packages* *component-packages*)
