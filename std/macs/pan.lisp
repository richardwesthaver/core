;;; pan.lisp --- Pandoric macros

;;; Code:
(in-package :std/macs)
(in-readtable :std)

(defun pandoriclet-get (letargs)
  "Primitive pandoric-get access to LETARGS."
  `(case sym
     ,@(mapcar #`(((car ,a1)) (car ,a1))
        letargs)
     (t (error
         "Unknown pandoric get: ~a"
         sym))))

(defun pandoriclet-set (letargs)
  "Primitive pandoric-set access to LETARGS."
  `(case sym
     ,@(mapcar #`(((car ,a1))
                  (setq (car ,a1) val))
        letargs)
     (t (error
         "Unknown pandoric set: ~a"
         sym))))

(defmacro pandoriclet (letargs &rest body)
  "Let-bind LETARGS and return a dlambda where they may be accessed via GET-PANDORIC."
  (let ((letargs (cons
                  '(%a)
                  (std/list:let-binding-transform
                   letargs))))
    `(let (,@letargs)
       (setq %a ,@(last body))
       ,@(butlast body)
       (dlambda
        (:pandoric-get (sym)
                       ,(pandoriclet-get letargs))
        (:pandoric-set (sym val)
                       ,(pandoriclet-set letargs))
        (t (&rest args)
           (apply %a args))))))

(declaim (inline get-pandoric))

(defun get-pandoric (box sym)
  "Get pandoric value SYM out of BOX."
  (funcall box :pandoric-get sym))

(defsetf get-pandoric (box sym) (val)
  "Set pandoric value of SYM in BOX."
  `(progn
     (funcall ,box :pandoric-set ,sym ,val)
     ,val))


(defmacro! with-pandoric (syms o!box &rest body)
  "Binds SYMS by calling GET-PANDORIC on BOX around BODY."
  `(symbol-macrolet
       (,@(mapcar #`(,a1 (get-pandoric ,g!box ,a1))
                  syms))
     ,@body))

(defmacro pandoric-recode (vars box new)
  "Recode the pandoric BOX binding VARS to NEW."
  `(with-pandoric (%a ,@vars) ,box
     (setq %a ,new)))

(defmacro plambda (largs pargs &rest body)
  "Define a pandoric lambda with lambda args LARGS and pandoric args PARGS."
  (let ((pargs (mapcar #'list pargs)))
    `(let (%a %p)
       (setq
        %a (lambda ,largs ,@body)
        %p (dlambda
              (:pandoric-get (sym)
                             ,(pandoriclet-get pargs))
              (:pandoric-set (sym val)
                             ,(pandoriclet-set pargs))
              (t (&rest args)
                 (apply %a args)))))))

(defvar pandoric-eval-tunnel)

(defmacro pandoric-eval (vars expr)
  "Evaluate pandoric expression EXPR using VARS bindings."
  `(let ((pandoric-eval-tunnel
           (plambda () ,vars t)))
     (eval `(with-pandoric
              ,',vars pandoric-eval-tunnel
              ,,expr))))
