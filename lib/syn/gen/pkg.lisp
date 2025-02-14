;;; pkg.lisp --- Code Generation Packages

;; Codegen Packages

;;; Commentary:

;; The SYN/GEN system contains code generators for Blub langs.

;;; Code:
(defpackage :syn/gen
  (:use :cl :std :doc :id :graph :sxp :ast)
  (:export :gen-designator :gen-condition :gen-condition
   :simple-gen-error :defsyntax
   :function-call :source-location
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

(defpackage :syn/gen/cli
  (:use :cl :std :syn/gen :cli :log)
  (:export :*gen-cli*))

