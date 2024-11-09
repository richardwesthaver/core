;;; var.lisp --- Codegen Variables

;; 

;;; Code:
(in-package :syn/gen)
(defvar *gen* nil)

(eval-always
  (defvar *gen-designators* (list :c :cpp :cu :rs :py :js :zig :cl :el :scm)))

(deftype gen-designator () `(or (member ,@*gen-designators*) null))
