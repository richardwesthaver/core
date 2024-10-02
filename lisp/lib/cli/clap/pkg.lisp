;;; cli/clap/pkg.lisp --- Clap Package Definitions

;; 

;;; Code:
(defpackage :cli/clap/vars
  (:use :cl)
  (:export :*cli-group-separator* :*no-exit* :*default-cli-def*
   :*default-cli-class* :*cli-opt-kinds* :*cli* :*opts*
   :*args* :*argc* :*arg* :*optc*))

(defpackage :cli/clap/util
  (:use :cl :std :log :sb-ext :cli/clap/vars)
  (:export :args :arg0 :long-opt-p
   :short-opt-p :opt-group-p :opt-string-prefix-eq :cli-opt-kind-p
   :default-thunk
   :long-opt-has-eq-p
   :opt-keyword-p))

(defpackage :cli/clap/macs
  (:use :cl :std :log :sb-ext :cli/clap/util :cli/clap/vars)
  (:export :defopt :defcmd
   :make-opt-parser :with-cli-handlers :make-shorty
   :with-opt-restart-case))

(defpackage :cli/clap/proto
  (:use :cl :std :log :sb-ext)
  (:import-from :cli/clap/util :args)
  (:export :proc-args :clap-error :find-short-opts
   :find-cmd :find-opts :parse-args :print-help
   :print-usage :print-version :do-cmds :do-cmd
   :active-cmds :active-opts :call-opt :do-opt
   :push-cmd :push-opt :cli-equal
   :do-opts
   :clap-simple-error
   :clap-simple-warning
   :clap-warning
   :clap-unknown-argument
   :handle-unknown-opt
   :handle-missing-opt
   :handle-invalid-opt
   :handle-unknown-arg
   :handle-missing-arg
   :handle-invalid-arg
   :clap-missing-argument
   :clap-invalid-argument
   :activate-cmd
   :activate-opt
   :find-opt
   :cli-args
   :opts
   :cmds))

(defpackage :cli/clap/ast
  (:use :cl :std :log :dat/sxp)
  (:export :cli-node :make-cli-node :cli-ast
   :make-cli-ast :cli-node-kind :cli-node-form))

(defpackage :cli/clap/obj
  (:use :cl :std :log
   :sb-ext :cli/clap/proto :cli/clap/macs :cli/clap/util
   :cli/clap/vars :cli/clap/ast :cli/clap/util)
  (:import-from :dat/sxp :ast)
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
   :cli-name))

(defpackage :cli/clap/simple
  (:use :cl :std :log :sb-ext)
  (:import-from :cli/ansi :.ris)
  (:import-from :uiop :println)
  (:import-from :sb-ext :parse-native-namestring)
  (:shadowing-import-from :sb-ext :exit)
  (:export))

(pkg:defpkg :cli/clap
  (:nicknames :clap)
  (:use-reexport :cli/clap/proto :cli/clap/obj :cli/clap/vars
   :cli/clap/simple :cli/clap/util :cli/clap/macs :cli/clap/ast
   :cli/clap/vars))
