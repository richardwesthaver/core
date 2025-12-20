;;; cpp/pkg.lisp --- C++ Code Generator

;; 

;;; Code:
(defpackage :syn/gen/cpp
  (:nicknames :gen/cpp)
  (:use :syn/gen/c :syn/gen :std/pipe :std/seq :std/meta :cli/tools/cc :cli/env :id :ast)
  (:shadow :delete :class :catch :decompose-declaration :function)
  (:import-from :syn/gen/c :*c-backend* :*c-symbols*)
  (:export
   #:*cpp-backend*
   #:*cpp-symbols*
   #:*cpp-syntax*
   #:*cpp-exports*
   #:*cpp-swap*))

(in-package :syn/gen/cpp)

(defmethod load-gen ((self (eql :cpp))) 
  (init-gen :cpp) 
  ;; (cpp-reader)
  )

(defmethod unload-gen ((self (eql :cpp))) (init-gen nil) (cl-reader))
(defmethod gen-package ((self (eql :cpp))) (find-package :syn/gen/cpp))
(defparameter *cpp-backend*
  (append *c-backend*
          '(delete new instantiate from-namespace
            template using using-namespace namespace
            access-specifier initializer constructor
            superclasses class attribute superclass
            declaration-list-initializer list-items)))

(defparameter *cpp-symbols* (append *c-symbols* '(delete decl struct for)))

(defparameter *cpp-syntax*
  (append *c-syntax*
          '(class vector new
            constructor destructor
            private public protected
            namespace using reference-type
            using-namespace from-namespace
            template instantiate
            for-each
            dynamic-cast static-cast
            reinterpret-cast const-cast)))

(defparameter *cpp-exports*
  (append *cpp-symbols*
          *cpp-syntax*
          *cl-symbols*))

(defparameter *cpp-swap*
  (append *cpp-symbols* *cpp-syntax*))

(pkg:defpackage* :syn/gen/cpp/swap
  (:shadow-symbols *cpp-swap*))

(pkg:defpackage* :syn/gen/cpp/sym
  (:shadow-symbols () :export-symbols *cpp-exports*)
  (:shadow :class :delete :vector :throw :catch :function 
           :for :struct)
  (:import-from :syn/gen/cpp :decompose-declaration)
  (:shadowing-import-from :syn/gen/c
   :gen-reader :gen-reader-switch
   :cl-reader :c-reader)
  (:use :syn/gen/c/sym :syn/gen/cpp))

(define-gen-backend :cpp :syn/gen/cpp :sym :syn/gen/cpp/sym :swap :syn/gen/cpp/swap)
