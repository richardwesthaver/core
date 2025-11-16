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
(defpkg :cli-int 
  (:use :cl :std) 
  (:export :*cli-packages* :*cli-tool-packages* :*cli-clap-packages*))

(in-package :cli-int)

(defparameter *cli-clap-packages* nil)
(setq *defpkg-hook* (lambda (x) (pushnew (package-name x) *cli-clap-packages* :test 'string=)))

(defpkg :cli/clap/vars
  (:use :cl)
  (:export :*cli-group-separator* :*no-exit* :*default-cli-def*
   :*default-cli-class* :*cli-opt-types* :*cli* :*opts*
   :*args* :*argc* :*arg* :*optc*
   :*cli-package-table*
   :*no-debug*))

(defpkg :cli/clap/util
  (:use :cl :std :log :sb-ext :cli/clap/vars)
  (:export :args :arg0 :long-opt-p
   :short-opt-p :group-opt-p :opt-string-prefix-eq :cli-opt-type-p
   :long-opt-has-eq-p
   :opt-keyword-p
   :short-opt-has-eq-p
   :default-cmd-thunk
   :default-opt-thunk))

(defpkg :cli/clap/macs
  (:use :cl :std :log :sb-ext :cli/clap/util :cli/clap/vars)
  (:export :defopt :defcmd :defopts
   :make-opt-parser :with-cli-handlers :make-shorty
   :argp
   :parse-cli-lambda-list))

(defpkg :cli/clap/proto
  (:use :cl :std :log :sb-ext)
  (:import-from :cli/clap/util :args)
  (:export :proc-args :clap-error :find-short-opts
   :find-cmd :find-opts :parse-args :print-help
   :print-usage :print-version :do-cmds :do-cmd
   :active-cmds :active-opts :call-opt :do-opt
   :push-cmd :push-opt
   :do-opts :clap-simple-error
   :clap-simple-warning :clap-warning
   :clap-unknown-argument :clap-missing-argument
   :clap-invalid-argument :activate-cmd
   :activate-opt :find-opt
   :cli-args :opts
   :cmds))

(defpkg :cli/clap/ast
  (:use :cl :std :log :obj/ast)
  (:export :cli-node :make-cli-node :cli-ast
   :make-cli-ast :cli-node-type :cli-node-form))

(defpkg :cli/clap/obj
  (:use :cl :std :log
   :sb-ext :cli/clap/proto :cli/clap/macs :cli/clap/util
   :cli/clap/vars :cli/clap/ast :cli/clap/util)
  (:import-from :equiv :equiv)
  (:import-from :obj/ast :ast :form :*ast*)
  (:export :make-cli :define-cli
   :make-opts :make-cmds :parse-boolean-opt :parse-string-opt
   :parse-form-opt :parse-list-op :parse-sym-op :parse-key-op
   :pasre-num-op :parse-file-op :parse-dir-op :cli
   :cli-cd :with-cli :debug-opts
   :cli-opt :cli-cmd :cli-opt-val :cli-opt-lock :cli-opt-name
   :active-cmds
   :%compose-keyword-opt
   :cli-cmd-args
   :getopt
   :setopt
   :add-package-cmd
   :add-package-opt
   :package-cli
   :package-cmds
   :package-opts
   :with-cli-args
   :load-package-cli
   :add-package-cmds
   :add-package-opts
   :help-opt
   :version-opt
   :level-opt
   :keep-ast-opt))

(defparameter *cli-packages* nil)

(setq *defpkg-hook* (compile nil (lambda (x) (pushnew (package-name x) *cli-packages* :test 'string=))))

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
  (:nicknames :ti :terminfo)
  (:use :cl :config)
  (:shadow :columns)
  (:import-from :std :winsize :+tiocgwinsz+)
  (:export
   :*terminfo-directories* :*terminfo*
   :capability :tparm :tputs :decode-padding
   :set-terminal :capabilities))

(defpkg :cli/linedit
  (:nicknames :linedit)
  (:use :cl :std)
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
   #:*announce*))

(defpkg :cli/progress
  (:use :cl :std)
  (:nicknames :progress)
  (:export
   :update-progress
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
  (:use :cl :std :cli/progress :cli/spark :config :ast :color)
  (:export :load-acl-repl :start-rl-repl :make-toplevel-init))

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

(defpkg :cli/main
  (:use :cl :std)
  (:import-from :cli/clap/vars :*no-exit* :*no-debug*)
  (:import-from :cli/clap/macs :with-cli-handlers)
  (:export
   #:defmain
   #:define-multi-main
   #:make-symlinks))

(defpkg :cli/tui
  (:use :cl :std :ansi :linedit :progress :spark :terminfo :env)
  (:export :completing-read :completing-read-form :defprompt))

(setq *defpkg-hook* nil)
