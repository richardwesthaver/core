;;; rule.lisp --- Skel Rule Objects

;; 

;;; Commentary:

;; TODO 2026-05-16: impl cmd protocol - RULE commands associated with projects.

;;; Code:
(in-package :skel/core)

;;; Rules
;; (defmethod sk-new ((self (eql :rule)) &rest args)
;;   (declare (ignore self))
;;   (apply #'sk-new 'sk-rule args))

(defmethod write-ast ((self rule) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(sink self) ,(source self) ,@(ast self)) :stream stream :pretty pretty :case case :readably t :array t :escape t))

(defmethod print-object ((self rule) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (sink self))
    (when-let ((source (source self)))
      (format stream " ~{~(~A~)~}" source))))

(eval-always
  (defmacro with-sk-rule-env (binds &body body)
    `(let (,@binds)
       (declare (ignorable ,@(mapcar 'car binds)))
       (symbol-macrolet ,*skel-project-symbol-macros*
         (macrolet ,*skel-project-macros*
           (labels ,*skel-project-functions*
             ,@body))))))

;(mapcar (lambda (x) (eval (cadr x))) binds)
;; Note that EXEC directly on a rule currently does NOT touch the sources.
(defmethod exec ((self rule))
  (compile-and-eval
   `(with-sk-rule-env ,(bind *project*)
      ,@(ast self))))

(defmethod write-object ((self rule) stream &key)
  (write-string (sink self) stream) ;; target isn't typep SK-OBJECT
  (write (source self) :stream stream)
  (write (ast self) :stream stream))

;; FIX 2025-06-09: 
(eval-always
  (defun make (obj &rest rules)
    (if rules
        (mapc
         (lambda (r) 
	       (when-let ((rule (project-find r obj)))
	         (call obj rule)))
         rules)
        (unless (sequence:emptyp (rules obj))
	      (let ((rule (aref (rules obj) 0)))
	        (if (source rule)
	            (make obj rule)
	            (exec rule)))))))

(defmethod call (self (rule rule))
  (when-let ((sources (and rule (source rule))))
    (mapcar
     (lambda (src)
       (if-let* ((sr (project-find src self)))
		 ;; TODO: check if we need to rerun sources
		 (make self sr)
		 (error "unhandled source: ~A for rule ~A" src rule)))
     sources))
  (exec rule))
