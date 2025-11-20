;;; sugar.lisp --- Syntactic Sugar Macros

;; Utility macros to make our code a bit more sweet.

;;; Code:
(in-package :std/macs)

(defmacro def! (name &body body)
  "`defun' without args."
  `(defun ,name () ,@body))

(defmacro deftyped* (name args &body body)
  "function definition with typed args."
  `(defun ,name ,(mapcar (lambda (x) (if (atom x) x (car x))) args) 
     (declare (ftype (function ,(mapcar (lambda (x) (or (cdr x) t)) args)) ,name))
     ,@body))

(defmacro deftyped (name args ret &body body)
  "function definition with typed args and return value."
  `(defun ,name ,(mapcar (lambda (x) (if (atom x) x (car x))) args) 
     (declare (ftype (function ,(mapcar (lambda (x) (or (cdr x) t)) args) ,ret) ,name))
     ,@body))

(defmacro defityped* (name args &body body)
  "inline function definition with typed args."
  `(definline ,name ,(mapcar (lambda (x) (if (atom x) x (car x))) args)
     (declare (ftype (function ,(mapcar (lambda (x) (or (cdr x) t)) args)) ,name))
     ,@body))

(defmacro defityped (name args ret &body body)
  "function definition with typed args and return value."
  `(definline ,name ,(mapcar (lambda (x) (if (atom x) x (car x))) args)
     (declare (ftype (function ,(mapcar (lambda (x) (if (atom x) t (cadr x))) args) ,ret) ,name))
     ,@body))

;; from lparallel
(defmacro defonce (name params &body body)
  "Like `defmacro' except that params which are immediately preceded
by `&once' are passed to a `once-only' call which surrounds `body'."
  (labels ((once-keyword-p (obj)
             (and (symbolp obj) (equalp (symbol-name obj) "&once")))
           (remove-once-keywords (params)
             (mapcar (lambda (x) (if (consp x) (remove-once-keywords x) x))
                     (remove-if #'once-keyword-p params)))
           (grab-once-param (list)
             (let ((target (first list)))
               (when (or (null list)
                         (consp target)
                         (find target lambda-list-keywords)
                         (once-keyword-p target))
                 (error "`&once' without parameter in ~a" name))
               target))
           (find-once-params (params)
             (mapcon (lambda (cell)
                       (destructuring-bind (elem &rest rest) cell
                         (cond ((consp elem)
                                (find-once-params elem))
                               ((once-keyword-p elem)
                                (list (grab-once-param rest)))
                               (t
                                nil))))
                     params)))
    (multiple-value-bind (body declares docstring) 
        (parse-body body :documentation t)
      `(defmacro ,name ,(remove-once-keywords params)
         ,@docstring
         ,@declares
         (once-only ,(find-once-params params)
           ,@body)))))

;;; Constants
(defun %reevaluate-constant (name value test)
  (if (not (boundp name))
      value
      (let ((old (symbol-value name))
            (new value))
        (if (not (constantp name))
            (prog1 new
              (cerror "Try to redefine the variable as a constant."
                      "~@<~S is an already bound non-constant variable ~
                       whose value is ~S.~:@>" name old))
            (if (funcall test old new)
                old
                (restart-case
                    (error "~@<~S is an already defined constant whose value ~
                              ~S is not equal to the provided initial value ~S ~
                              under ~S.~:@>" name old new test)
                  (ignore ()
                    :report "Retain the current value."
                    old)
                  (continue ()
                    :report "Try to redefine the constant."
                    new)))))))

(defmacro define-constant (name initial-value &key (test ''eql) documentation)
  "Ensures that the global variable named by NAME is a constant with a value
that is equal under TEST to the result of evaluating INITIAL-VALUE. TEST is a
/function designator/ that defaults to EQL. If DOCUMENTATION is given, it
becomes the documentation string of the constant.

Signals an error if NAME is already a bound non-constant variable.

Signals an error if NAME is already a constant variable whose value is not
equal under TEST to result of evaluating INITIAL-VALUE."
  `(progn
     (defconstant ,name (%reevaluate-constant ',name ,initial-value ,test)
       ,@(when documentation `(,documentation)))))

;;; Vars
;; from HUNCHENTOOT
(defmacro defvar-unbound (name &optional (doc-string ""))
  "Convenience macro to declare unbound special variables with a
documentation string."
  `(progn
     (defvar ,name)
     (setf (documentation ',name 'variable) ,doc-string)
     ',name))

;;; Eval
(defmacro eval-always (&body body)
  "Eval BODY in all contexts (:compile-toplevel :load-toplevel :execute)."
  `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body))

(defmacro eval-every (&body forms)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@forms))

(defun compile-and-eval (form)
  "Compile and eval a FORM."
  (funcall (compile nil `(lambda () ,form))))

(defun compile-and-eval* (form)
  "Splice, compile, and eval a FORM."
  (funcall (compile nil `(lambda () ,@form))))

(defun compile-and-load (file &key (output-file ""))
  "Utility function which compiles a lisp FILE and loads the resulting fasl file."
  (load (compile-file (pathname file) :output-file output-file)))

;; from jackdaniel's Dynamic Slots, see also META/DYNAMIC
(defmacro dlet (bindings &body body)
  "LET form -> PROGV form."
  (loop for (var val) in bindings
        collect var into vars
        collect val into vals
        finally (return `(progv (list ,@vars) (list ,@vals)
                           ,@body))))

;;; Definitions
;; TODO 2025-08-12: 
;; inspired by LFARM
(defmacro defwith (name args &rest bind)
  "Define a call-with-NAME function and with-NAME macro which accept ARGS.

NOTE: currently the bindings are completely unevaluated - if you pass (SELF)
to ARGS you should have something like (FOO (EVAL SELF)) in BIND."
  (let ((wname (symbolicate "WITH-" name))
        (cwname (symbolicate "CALL-WITH-" name)))
    `(progn
       (defun ,cwname (,@args body-fn)
         (let (,@bind)
           (declare (special ,@(mapcar 'car bind)))
           (funcall body-fn)))
       (defmacro ,wname (,@args &body body)
         `(,',cwname ',,@args (lambda () ,@body))))))

;; TODO 2024-10-24: 
(defmacro defclass* (name direct-superclasses direct-slots &rest opts)
  "Convenience wrapper for DEFCLASS - always binds the following slot args to
default values unless overwritten at runtime:

:INITARG
:ACCESSOR

The following additional options are supported:
:METHOD - specify methods with default bindings
:SER - serializer slot specification
:DE - deserializer slot specification
:SERIALIZE - define a serizlier
:DESERIALIZE - define a deserializer
:HOOK - hook slot specification
:COPY - COPY-OBJECT method definition
:KERNEL - make this a kernel (funcallable) class
:VERB - define verb methods
:TYPE-ID - register a type-id for instances of this class"
  `(defclass ,name ,direct-superclasses 
     ,(mapcar 
       (lambda (x) 
         (etypecase x
           (atom `(,x :initarg ,(sb-int:keywordicate x) :accessor ,(sb-int:symbolicate name '- x)))
           (cons 
            (let ((%name (car x))
                  (%args (cdr x)))
              `(,%name ,@(std:acond
                          ((getf %args :initarg)
                           (remf %args :initarg)
                           (if (getf %args :accessor)
                               `(:initarg ,it ,@%args)
                               (if (getf %args :reader)
                                   `(:initarg ,it ,@%args)
			           `(:initarg ,it :accessor ,(sb-int:symbolicate name '- x) ,@%args))))
			  ((getf %args :accessor)
			   (remf %args :accessor)
			   (if (getf %args :intitarg)
			       `(:accessor ,it ,@%args)
			       `(:accessor ,it :initarg ,(keywordicate %name) ,@%args)))))))))
       direct-slots)
     ,@opts))

;; Based on INCONGRUENT-METHODS:DEFINE-CLASS
(defmacro define-class (name direct-superclasses direct-slots &body body)
  "Like DEFCLASS but with the forms in BODY acting as simplified method
definitions."
  (with-gensyms (self)
    (labels ((slot-definition (x)
               (if (listp x)
                   (cons (first x)
                         (remove-from-plist (rest x)
                                            :reader :writer :accessor))
                   x))
             (slot-accessor-definition (x)
               (destructuring-bind (slot-name &rest options) x
                 (loop :for (options value) :on options :by #'cddr
                       :append
                          (case options
                            (:accessor
                             `((defmethod ,value ((,self ,name))
                                 (slot-value ,self ',slot-name))
                               (defmethod (setf ,value)
                                   (new (,self ,name))
                                 (setf (slot-value ,self ',slot-name) new))))
                            (:reader
                             `((defmethod ,value ((,self ,name))
                                 (slot-value ,self ',slot-name))))
                            (:writer
                             `((defmethod (setf ,value)
                                   (new (,self ,name))
                                 (setf (slot-value ,self ',slot-name) new))))))))
             (method-definition (definition)
               (destructuring-bind (method-name lambda-list &rest body)
                   definition
                 (if (listp method-name)
                     `(define-class-method ,method-name (,(first lambda-list)
                                                         (,(intern "SELF") ,name)
                                                         ,@(rest lambda-list))
                        ,@body)
                     `(define-class-method ,method-name ((,(intern "SELF") ,name)
                                                         ,@lambda-list)
                        ,@body)))))
      `(progn
         (defclass ,name ,direct-superclasses
           ,(mapcar #'slot-definition direct-slots))
         ,@(mapcan #'slot-accessor-definition
                   (remove-if-not #'listp direct-slots))
         ,@(mapcar #'method-definition body)))))

;;; Nest
;; I don't use this much, but it is quite handy.
;; ref: https://fare.livejournal.com/189741.html

;; in this case we just pull the version from UIOP.

;; Thanks to mbaringer
(defmacro nest (&rest things)
  "Macro to keep code nesting and indentation under control." 
  (reduce #'(lambda (outer inner) `(,@outer ,inner))
          things :from-end t))

;;; Let extensions
(defmacro letv* (bindings &rest body)
  "Extended LET* which handles multiple values, destructuring bind, and type declarations. 

The declarations list VARS is similar to that in let.

Examples:
(macroexpand-1 
 `(letv* ((x 2 :type fixnum)
         ((a &optional (c 2)) b (values (list 1) 3) :type (fixnum &optional (t)) t))
   t))
;; (LET ((X 2))
;;       (DECLARE (TYPE FIXNUM X))
;;   (MULTIPLE-VALUE-BIND (#:G1120 B) (VALUES (LIST 1) 3)
;;     (DECLARE (TYPE T B))
;;     (DESTRUCTURING-BIND (A &OPTIONAL (C 2)) #:G1120
;;       (DECLARE (TYPE FIXNUM A)
;;                (TYPE T C))
;;       (LOCALLY T))))"
  (with-gensyms (consy)
  (labels ((typedecl (syms alist)
	     (let ((decls (remove-if #'null (mapcar 
                                                    #'(lambda (s)
							(let ((ts (assoc s alist)))
                                                          (when ts
							    (if (second ts)
							        `(type ,(second ts) ,s)
							        `(ignore ,s)))))
						    syms))))
	       (when decls `((declare ,@decls))))))
    (apply #'recursive-append
	   (append
	    (mapcan #'(lambda (x)
			(destructuring-bind (bind expr type) (let ((tpos (position :type x)) (len (length x)))
							       (list (std/list:deconsify (subseq x 0 (1- (or tpos len))) consy) (nth (1- (or tpos len)) x) (when tpos (std/list:deconsify (nthcdr (1+ tpos) x) consy))))
			  (let* ((typa (std/list:maptree t #'(lambda (x) (if (atom (car x))
                                                                            (unless (or (eql (car x) consy) (member (car x) cl:lambda-list-keywords)) (list x))
                                                                            (values x #'(lambda (mf x) (apply #'append (mapcar mf x))))))
                                                         (std/list:ziptree bind type)))
			         (vsyms (mapcar #'(lambda (x) (if (listp x)
								  (let ((g (gensym)))
								    (list g
									  `(destructuring-bind ,(std/list:reconsify x consy)
                                                                               ,g
									     ,@(typedecl (flatten x) typa))))
								  (list x)))
						bind)))
			    (list*
			     (recursive-append
			      (if (> (length bind) 1)
				  `(multiple-value-bind (,@(mapcar #'car vsyms)) ,expr)
				  `(let ((,@(mapcar #'car vsyms) ,expr))))
			      (car (typedecl (mapcar #'car vsyms) typa)))
			     (remove-if #'null (mapcar #'cadr vsyms))))))
		    bindings)
	    `((locally ,@body)))))))

(flet ((let-typed-expansion (letsym bindings body)
         (multiple-value-bind (body decl) (parse-body body)
           `(,letsym (,@(mapcar #'(lambda (x) (subseq x 0 2)) bindings))
                     ,@(let ((types (remove nil (mapcar #'(lambda (x) (destructuring-bind (s e &key (type t)) x
                                                                        (declare (ignore e))
                                                                        (unless (eql type t)
                                                                          (if (null type)
                                                                              `(ignore ,s)
                                                                              `(type ,type ,s)))))
                                                        bindings))))
                         (when (or decl types) `((declare ,@types ,@decl))))
                     ,@body))))
  (defmacro lety (bindings &body body)
    "
  This macro works basically like let, but also allows type-declarations
  with the key :type.

  Example:

  (macroexpand-1
      `(let-typed ((x 1 :type fixnum))
          (+ 1 x)))
  => (LET ((X 1))
        (DECLARE (TYPE FIXNUM X))
        (+ 1 X))"
    (let-typed-expansion 'let bindings body))

  (defmacro lety* (bindings &body body)
    "
  This macro works basically like let*, but also allows type-declarations
  with the key :type.

  Example:

  (macroexpand-1
      `(let*-typed ((x 1 :type fixnum))
          (+ 1 x)))
  => (LET* ((X 1))
        (DECLARE (TYPE FIXNUM X))
        (+ 1 X))"
    (let-typed-expansion 'let* bindings body)))
