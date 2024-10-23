;;; var.lisp --- Codegen Variables

;; 

;;; Code:
(in-package :syn/gen)
(defvar *generator* nil)
(eval-always
  (defvar *codegen-designators* (list :c :cpp :cu :rs :py :js :zig :cl :el :scm)))
(deftype codegen-designator () `(or (member ,@*codegen-designators*) null))
