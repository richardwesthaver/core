;;; pkg.lisp --- Code Generation Packages

;; Codegen Packages

;;; Commentary:

;; The SYN/GEN system contains code generators for all major languages we use - including other Lisps and Common Lisp itself.

;;; Code:
(defpackage :syn/gen
  (:use :cl :std :doc :id :graph :sxp :ast)
  (:export :gen-designator :gen-condition :gen-condition
   :simple-gen-error :defsyntax
   :function-call :source-location
   :ident :str-literal
   :num-literal :char-literal
   :load-generator :init-gen
   :*gen* :*gen-designators*
   :*cl-symbols* :*code-reader*
   :*backup-readtable* :define-code-switches
   :define-code-switch :print-code
   :define-code-processor :define-code-reader
   :make-nodes :make-node
   :generator-package :build-swap-package
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
   :make-proxy :del-proxy :info-size
   :empty))

(in-package :syn/gen)

;; TODO 2024-10-20: gen-file-header
