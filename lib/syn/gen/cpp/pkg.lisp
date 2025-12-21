;;; cpp/pkg.lisp --- C++ Code Generator

;; 

;;; Code:
(defpackage :syn/gen/cpp
  (:nicknames :gen/cpp)
  (:use :syn/gen :syn/gen/c :ast :id)
  (:shadow :delete :class :catch :cl-reader :decompose-declaration)
  (:import-from :syn/gen/c :*c-backend* :*c-symbols*)
  (:export
   :decompose-declaration
   :cpp-reader-switch :cpp-reader
   #:*cpp-backend*
   #:*cpp-symbols*
   #:*cpp-syntax*
   #:*cpp-exports*
   #:*cpp-swap*))

(in-package :syn/gen/cpp)

(defmethod gen-package ((self (eql :cpp))) (find-package :syn/gen/cpp/sym))

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
  (:use :syn/gen/c/sym)
  (:nicknames :cpp)
  (:shadow :class :delete :vector :throw :catch :function 
           :for :struct)
  (:import-from :syn/gen/cpp :decompose-declaration)
  (:shadowing-import-from :syn/gen/cpp
   :cpp-reader-switch :cl-reader :cpp-reader))

(define-gen-backend :cpp :syn/gen/cpp :sym :syn/gen/cpp/sym :swap :syn/gen/cpp/swap)
