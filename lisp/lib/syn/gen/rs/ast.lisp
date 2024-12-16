;;; ast.lisp --- GEN/RS AST Nodes

;; 

;;; Code:
(in-package :syn/gen/rs)

;;; Nodes

;;; Syntax
(defmacro rs-syntax (tags lambda-list &body body)
  `(defsyntax ,tags (:rs) ,lambda-list ,@body))

;;; Context Switches
(build-context-switches
 :package :syn/gen/rs/sym
 :symbols *rs-symbols*)

(build-swap-package
 :package :syn/gen/rs/sym
 :swap-package :syn/gen/rs/swap
 :symbols *rs-swap*)
