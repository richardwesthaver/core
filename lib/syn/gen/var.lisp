;;; var.lisp --- Codegen Variables

;; 

;;; Code:
(in-package :syn/gen)
(defvar *gen* nil)

(eval-always
  (defvar *gen-designators* (list :c :cpp :cu :py :js)))

(deftype gen-designator () `(or (member ,@*gen-designators*) null))

(defparameter *gen-backend-table* (make-hash-table))

(defparameter *code-reader* 'gen)
(defparameter *backup-readtable* (copy-readtable nil))

(defvar *gen-warnings* '(:hyphen))

(defvar *cl-symbols*
  (let ((syms))
    (do-external-symbols (s :common-lisp) (push s syms))
    syms))
