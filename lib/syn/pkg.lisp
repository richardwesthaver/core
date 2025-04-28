;;; lib/syn/pkg.lisp --- Syn Packages

;; Syntax Processors

;;; Commentary:

;;; Code:
(defpackage :syn/lint
  (:use :cl :std)
  (:export :lint))

(defpackage :syn/ts
  (:use :cl :std :tree-sitter)
  (:export 
   :parse-file
   :lang-counts))

(defpackage :syn/lang
  (:use :cl :std)
  (:export :language :lang :*language*))

(defpackage :syn
  (:use :cl :std :syn/lint :syn/ts :syn/lang))

(defpackage :syn/gen
  (:use :cl :std :doc :id :graph :sxp :ast)
  (:export :gen-designator :gen-condition :gen-condition
   :simple-gen-error :defsyntax
   :function-call :src-location
   :ident :str-literal
   :num-literal :char-literal
   :load-gen :init-gen
   :*gen* :*gen-designators*
   :*cl-symbols* :*code-reader*
   :*backup-readtable* :define-code-switches
   :define-code-switch :print-code
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
   :lisp))

(defpackage :syn/cli
  (:use :cl :std :syn/lint :syn/ts :syn/lang :cli :log :syn/gen)
  (:export :*syn-cli*
           :*gen-cli*))

(in-package :syn)
(in-package :syn/lang)
(defclass language () ())
(defgeneric lang (self))
(sb-ext:define-load-time-global *language* nil)
