;;; cli/clap/pkg.lisp --- Clap Package Definitions

;; 

;;; Code:
(in-package :cli/int)
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
  (:export :defopt :defcmd
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
  (:export :make-cli :define-cli :defmain
   :make-opts :make-cmds :parse-bool-opt :parse-string-opt
   :parse-form-opt :parse-list-op :parse-sym-op :parse-key-op
   :pasre-num-op :parse-file-op :parse-dir-op :cli
   :cli-cd :with-cli :debug-opts
   :cli-opt :cli-cmd :cli-opt-val :cli-opt-lock :cli-opt-name
   :active-cmds
   :%compose-keyword-opt
   :cli-cmd-args
   :cli-lock-p
   :cli-name
   :getopt
   :setopt
   :set-package-cli
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

(setq *defpkg-hook* nil)
