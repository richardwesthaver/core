;;; math/pkg.lisp --- Core Math Packages

;;

;;; Code:
(defpackage :math-int
  (:use :cl :std)
  (:export :*math-packages*))
(in-package :math-int)
(defparameter *math-packages* nil)
(setq *defpkg-hook* (lambda (x) (pushnew (package-name x) *math-packages* :test 'string=)))

(defpkg :math/util
  (:use :cl :std :tensor)
  (:import-from :cli/tools/cc :run-nvcc)
  (:import-from :cuda :device-compute-capability)
  (:export :blasfunc :lapackfunc :with-lapack-query :~
           :math-error :math-warning))

(defpkg :math/blas
  (:use :std-lisp :blas :tensor)
  (:import-from :math/util :blasfunc)
  (:export :axpy 
   :tensor-sum #:tensor-sum! #:prod! :mean 
   #:axpy! #:normalize!
   :prod #:ger! #:ger #:trs! #:gem! #:gem #:gett! #:gekr!
   ;; may want to export this from obj/tensor..
   :meshgrid))

(defpkg :math/lapack
  (:use :std-lisp :math/blas #+lapack :lapack :tensor)
  (:import-from :math/util :lapackfunc)
  (:export
   #:potrf! #:chol! #:chol #:potrs! #:potri! #:ldl! #:ldl-permute! #:ldl
   #:geev! #:geev-complexify-eigvec  #:heev! #:eig
   #:gelsy #:lstsq
   #:getrf! #:getrs! #:getri! #:lu
   #:qr! #:qr #:schur #:svd #:trsyl! #:syl))

(defpkg :math/cuda
  (:use :std-lisp :math/util :tensor :cuda)
  (:import-from :tensor :t.store-type :t.compute-store-size :t.store-size :foreign-vector-store-mixin
   :real-subtypep :field-type :store-type :t.total-size :t.store-ref :t.store-set :t.store-allocator
   :with-field-element :tensor-generator))

(defpkg :math/sfc
  (:use :std-lisp)
  (:export
   #:hilbert-list
   #:hilbert-curve))

(defpkg :math/auto
  (:use :std-lisp)
  (:export :life :cellular-automata :*rule-patterns*))

(defpkg :math/graph
  (:use :std-lisp :tensor)
  (:export))

(setq *defpkg-hook* nil)

(eval-always
  (defparameter *math-syntax*
    '(;;arithmetic
      #:+ #:- #:* #:.* #:/ #:./ #:@ #:· #:expt #:^ #:⊗ #:= #:.=
      ;;function
      #:sin! #:cos! #:tan! #:asin! #:acos! #:exp! #:sinh! #:cosh! #:tanh! #:asinh! #:acosh! #:atanh!
      #:sin #:cos #:tan #:asin #:acos #:exp #:sinh #:cosh #:tanh #:asinh #:acosh #:atanh
      #:log #:log! #:atan #:atan! #:expt #:expt!
      ;; #:transpose #:ctranspose
      #:sum #:realpart #:imagpart #:max #:min #:conjugate))

  (defparameter *math-exports* (append syn:*cl-symbols* *math-syntax*))

  (defpackage* :math/sym
    (:shadow-symbols *math-syntax* :export-symbols *math-exports*)
    (:use :cl :std :tensor :math/blas :math/lapack :math/cuda :id)))

(defpkg :math/syn
  (:shadowing-import-from :math/sym . #.*math-exports*)
  (:use :std-lisp :tensor :parse/yacc :id :math/blas :math/lapack :math/cuda :math/sym)
  (:export :*linfix-parser*))
