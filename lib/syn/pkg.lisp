;;; lib/syn/pkg.lisp --- Syn Packages

;; Syntax Processors

;;; Commentary:

;;; Code:
(defpkg :syn/ts
  (:nicknames :ts)
  (:use :cl :std :tree-sitter)
  (:export 
   :parse-file
   :parse-string
   :lang-counts))

(defpkg :syn/fmt
  (:use :cl :std :config :ast)
  (:export :fmt :fmt-config))

(defpkg :syn/gen
  (:use :cl :std :doc :id :graph :ast)
  (:export :gen-designator :gen-condition :gen-error
   :simple-gen-error :defsyntax
   :function-call :src-location
   :ident :str-literal
   :cintern
   :num-literal :char-literal
   :load-gen :init-gen
   :*gen* :*gen-langs*
   :*cl-symbols* :*code-reader*
   :*backup-readtable* :define-code-switches
   :define-code-switch :print-code
   :code-print
   :define-code-processor :define-code-reader
   :make-nodes :make-node
   :build-swap-package
   :build-context-switches :*gen-warnings*
   :ast-traverser :with-code-printer
   :define-code-printer :delete-code-printer
   :write-code :*code-dispatch-table*
   :code-printer :with-code-printer 
   :++indent :--indent 
   :stream :node
   :indent :pop-sign 
   :top-sign :push-sign 
   :find-sign :node-slot
   :pop-info :top-info 
   :push-info :find-info
   :make-proxy :del-proxy :info-size :empty
   :quoty
   :%level
   :%self
   :gen-reader
   :gen-reader-switch
   :gen-package
   :unload-gen
   :with-codegen
   :*indent*
   :tag
   :gen
   :lisp
   :*gen-backend-table*
   :define-gen-backend
   :gen-backend
   :cl-reader))

(defpkg :syn/lint
  (:use :std-lisp)
  (:export :lint))

(defpkg :syn/srv
  (:use :cl :std :config :ast :srv)
  (:export))

(defpkg :syn/lang
  (:use :cl :std :config :ast :id :project :cmd :tree-sitter :syn/ts)
  (:export :lang :*lang* :lang-config 
   :deflang :lang-condition :lang-error :lang-warning
   :*langs* :langp :with-lang :lang-stats))

(defpkg :syn/lang/c
  (:nicknames :syn/c)
  (:use :cl :std :syn/lang :parse/pratt :tree-sitter :syn/ts)
  (:export))

(defpkg :syn/lang/js
  (:nicknames :syn/js)
  (:use :cl :std :syn/lang :tree-sitter)
  (:export))

(defpkg :syn/lang/py
  (:use :cl :std :syn/lang :tree-sitter :syn/ts)
  (:export))

(defpkg :syn/lang/rs
  (:nicknames :syn/rs)
  (:use :cl :std :syn/lang :tree-sitter :syn/ts)
  (:export))

(defpkg :syn/tempo
  (:nicknames :tempo)
  (:use :std-lisp :syn/ts :syn/lang :syn/gen :id :graph :ast :val)
  (:import-from :dat/xml :escape-for-html)
  (:import-from :url :url-encode)
  (:export :*tempo-start* :*tempo-end* :tempo-function 
   :make-tempo-function :expand-template-tags :*tempo-table* :register-template
   :getf-tempo :*tempo-package* :execute-template :deftempo
   :*tempo-parameters* :*tempo-variables* :*tempo-case-sensitive*))

(defpkg :syn/grovel
  (:use :cl :std :syn/ts :syn/lang :syn/tempo :syn/lint :syn/lang/c)
  (:export))

(defpkg :syn
  (:use :std-lisp)
  (:use-reexport :syn/lint :syn/ts :syn/lang :syn/gen :syn/tempo :syn/grovel))

(defpkg :syn/cli
  (:use :std-lisp :syn :cli :clap :log)
  (:export :*syn-cli*
           :*gen-cli*))
