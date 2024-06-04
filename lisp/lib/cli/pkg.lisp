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
(defpackage :cli/shell
  (:use :cl :std)
  (:nicknames :shell)
  (:export :*shell* :*shell-directory* :*shell-input*))

(defpackage :cli/ansi
  (:use :cl :std)
  (:nicknames :ansi)
  (:export
   ;; ESC sequences
   :.ris :reset-to-initial-state
   ;; CSI sequences
   ;; Cursor control
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

(defpackage :cli/prompt
  (:use :cl :std)
  (:export
   :completing-read
   :defprompt))
   
(defpackage :cli/progress
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
   :progress-bar))

(defpackage :cli/spark
  (:use :cl :std)
  (:export
   :spark :*ticks*
   :vspark :*vticks*))

(defpackage :cli/repl
  (:use :cl :std :cli/progress :cli/spark)
  (:export :load-acl-repl :start-rl-repl))

(defpackage :cli/ed
  (:use :cl :std)
  (:export :run-emacs :run-emacsclient
           :org-store-link))

(defpackage :cli/clap
  (:nicknames :clap)
  (:use :cl :std :log :sb-ext)
  (:import-from :cli/ansi :.ris)
  (:import-from :uiop :println)
  (:import-from :sb-ext :parse-native-namestring)
  (:shadowing-import-from :sb-ext :exit)
  (:export
   :*no-exit*
   :arg0
   :args
   :*cli-group-separator*
   :*cli-opt-kinds*
   :cli-opt-kind-p
   :global-opt-p
   :*std-local-env-var-names*
   :*std-global-env-var-names*
   :exec-path-list
   :program-list
   :find-exe
   :ld-library-path-list
   :argp
   :$val
   :$args
   :$argc
   :$opts
   :$optc
   :make-shorty
   :with-cli-handlers
   :defmain
   :with-cli
   :make-cli
   ;; opt-parsers
   :make-opt-parser
   :parse-bool-opt
   :parse-str-opt
   :parse-form-opt
   :parse-list-opt
   :parse-sym-opt
   :parse-key-opt
   :parse-num-opt
   :parse-file-opt
   :parse-dir-opt
   :make-opts
   :make-cmds
   :active-opts
   :active-cmds
   :proc-args
   :make-cli-node
   :make-cli-ast
   :proc-args
   :parse-args
   :debug-opts
   :do-cmd
   :do-opt
   :call-opt
   :call-cmd
   :apply-cmd
   :print-help
   :print-version
   :print-usage
   :handle-unknown-argument
   :handle-missing-argument
   :handle-invalid-argument
   :cli-opt
   :cli-opt-val
   :cli-val
   :cli-cmd-args
   :cli-cmd
   :cli-cd
   :find-cmd
   :find-opts
   :find-short-opts
   :install-ast
   ;; :gen-cli-thunk
   :install-thunk
   :cli
   :cli-equal
   :defopt
   :defcmd
   :define-cli
   ;; ast types
   :opt
   :cmd
   ;; :arg
   :cli-name
   :cli-opts
   :cli-cmds
   :cli-thunk
   :cli-description
   :cli-version
   :cli-usage))

(defpackage :cli/tmux
  (:use :cl :std)
  (:export))

(in-package :std-user)

(defpkg :cli
  (:use :cl :std)
  (:use-reexport :cli/shell :cli/ansi :cli/prompt
   :cli/progress :cli/spark :cli/prompt :cli/ed
   :cli/repl :cli/clap))

(defpkg :cli-user (:use :cl :std :cli))
  
