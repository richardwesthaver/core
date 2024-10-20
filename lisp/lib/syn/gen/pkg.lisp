;;; pkg.lisp --- Code Generators

;; 

;;; Code:
(defpackage :syn/gen
  (:use :cl :std :doc)
  (:export :codegen-designator :codegen-condition :codegen-condition
   :simple-codegen-error))
(in-package :syn/gen)

;;; Special
(defvar *generator nil)

;;; Conditions
(eval-always
  (define-condition codegen-condition () ())
  (defvar *codegen-designators* (list :c :rs :py :js))
  (deftype codegen-designator () `(member ,@*codegen-designators*)))

(deferror codegen-error (codegen-condition) () (:auto t))
(deferror simple-codegen-error (codegen-error simple-error) () (:auto t))

;; TODO 2024-10-20: codegen-file-header

;;; Packages

(defpackage :syn/gen/el
  (:nicknames :genel :el)
  (:use :cl :syn/gen))

(defpackage :syn/gen/scm
  (:nicknames :genscm :scm)
  (:use :cl :syn/gen))

(defpackage :syn/gen/c
  (:nicknames :genc :c)
  (:use :cl :syn/gen))

(defpackage :syn/gen/cu
  (:nicknames :gencu :cu)
  (:use :cl :syn/gen))

(defpackage :syn/gen/cpp
  (:nicknames :gencpp :cpp)
  (:use :cl :syn/gen))

(defpackage :syn/gen/zig
  (:nicknames :genzig :zig)
  (:use :cl :syn/gen))

(defpackage :syn/gen/rs
  (:nicknames :genrs :rs)
  (:use :cl :syn/gen)
  (:import-from :std :in-readtable :eval-always))

(defpackage :syn/gen/py
  (:nicknames :genpy :py)
  (:use :cl :syn/gen))

(defpackage :syn/gen/js
  (:nicknames :genjs :js)
  (:use :cl :syn/gen))
