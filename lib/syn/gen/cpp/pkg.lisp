;;; cpp/pkg.lisp --- C++ Code Generator

;; 

;;; Code:
(defpackage :syn/gen/cpp
  (:nicknames :gen/cpp)
  (:use :cl :syn/gen :syn/gen/c :std/seq :std/meta :std/pipe)
  (:export
   #:*cpp-backend*))

(defpackage :syn/gen/cpp/swap)

(pkg:defpkg :syn/gen/cpp/sym
  (:use :syn/gen/c/sym :syn/gen/cpp))

(in-package :syn/gen/cpp)

(defmethod load-gen ((self (eql :cpp))) 
  (init-gen :cpp) 
  ;; (cpp-reader)
  )

(defmethod unload-gen ((self (eql :cpp))) (init-gen nil) (syn/gen/c:cl-reader))
(defmethod gen-package ((self (eql :cpp))) (find-package :syn/gen/cpp))
(defparameter *cpp-backend*
  (append *c-backend*
          '(delete new instantiate from-namespace
            template using using-namespace namespace
            access-specifier initializer constructor
            superclasses class attribute superclass
            declaration-list-initializer list-items)))
