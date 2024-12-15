;;; cpp/pkg.lisp --- C++ Code Generator

;; 

;;; Code:
(defpackage :syn/gen/cpp
  (:nicknames :gen/cpp)
  (:use :cl :syn/gen :syn/gen/c)
  (:export
   #:*cpp-backend*))

(pkg:defpkg :syn/gen/cpp/sym
  (:use :syn/gen/c/sym :syn/gen/cpp))

(in-package :syn/gen/cpp)

(defmethod load-generator ((self (eql :cpp))) :cpp)

(defparameter *cpp-backend*
  (append *c-backend*
          '(delete new instantiate from-namespace
            template using using-namespace namespace
            access-specifier initializer constructor
            superclasses class attribute superclass
            declaration-list-initializer list-items)))
