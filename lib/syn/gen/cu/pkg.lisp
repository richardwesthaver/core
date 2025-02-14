;;; cu/pkg.lisp --- CUDA Code Generator

;; 

;;; Code:
(defpackage :syn/gen/cu
  (:nicknames :gen/cu)
  (:use :cl :syn/gen :syn/gen/c :syn/gen/cpp)
  (:export
   #:*cu-backend*))

(pkg:defpkg :syn/gen/cu/sym
  (:use :cl :syn/gen/cu))

(in-package :syn/gen/cu)

(defmethod load-gen ((self (eql :cu))) :cu)
(defmethod gen-package ((self (eql :cu))) (find-package :syn/gen/cu))

(defparameter *cu-backend*
  (append *cpp-backend*
          '(size cuda-alignment shared threads
            blocks cuda-funcall)))
