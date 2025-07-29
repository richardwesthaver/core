;;; sugar.lisp --- Syntactic Sugar Macros

;; Utility macros to make our code a bit more sweet.

;;; Code:
(in-package :std/macs)

(defmacro def! (name &body body)
  "`defun' without args."
  `(defun ,name () ,@body))

(defmacro eval-always (&body body)
  "Eval BODY in all contexts (:compile-toplevel :load-toplevel :execute)."
  `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body))

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

(defun without-props (plist props)
  "Return a new PLIST with all keys in PROPS dropped."
  (loop for (options value) on plist by #'cddr
        append (unless (member options props)
                 (list options value))))

;; TODO 2024-10-24: 
(defmacro defclass* (name direct-superclasses direct-slots &rest opts)
  "Convenience wrapper for DEFCLASS - always binds the following slot args to
default values unless overwritten at runtime:

:INITARG
:ACCESSOR"
  `(defclass ,name ,direct-superclasses 
     ,(mapcar 
       (lambda (x) 
         (etypecase x
           (atom `(,x :initarg ,(sb-int:keywordicate x) :accessor ,(sb-int:symbolicate name '- x)))
           (cons 
            (let ((%name (car x))
                  (%args (cdr x)))
              `(,%name ,@(std:acond
                          ((getf x :initarg)
                           (remf x :initarg)
                           (if-let ((acc (getf x :accessor)))
                             (progn
                               (remf x :accessor)
                               `(:initarg ,it :accessor ,acc ,@%args))
			     `(:initarg ,it :accessor ,(sb-int:symbolicate name '- x) ,@%args)))
			  ((getf x :accessor)
			   (remf x :accessor)
			   (if-let ((acc (getf x :intargr)))
			     (progn
			       (remf x :initarg)
			       `(:accessor ,it :initarg ,acc ,@%args))
			     `(:accessor ,it :initarg ,%name ,@%args)))))))))
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
                         (without-props (rest x)
                           '(:reader :writer :accessor)))
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

;; in this case we just pull the version from UIOP
(defmacro nest (&rest things)
  "Macro to keep code nesting and indentation under control." ;; Thanks to mbaringer
  (reduce #'(lambda (outer inner) `(,@outer ,inner))
          things :from-end t))

;;; Let extensions
(defmacro letv* (bindings &rest body)
  "Extended LET* which handles multiple values, destructuring bind, and type declarations. 

The declarations list VARS is similar to that in let.

Examples:
(macroexpand-1 `(letv* ((x 2 :type fixnum)
                        ((a &optional (c 2)) b (values (list 1) 3) :type (fixnum &optional (t)) t))
                  t))
;; (LET ((X 2))
;;       (DECLARE (TYPE FIXNUM X))
;;   (MULTIPLE-VALUE-BIND (#:G1120 B) (VALUES (LIST 1) 3)
;;     (DECLARE (TYPE T B))
;;     (DESTRUCTURING-BIND (A &OPTIONAL (C 2)) #:G1120
;;       (DECLARE (TYPE FIXNUM A)
;;                (TYPE T C))
;;       (PROGN T))))"
  (labels ((typedecl (syms alist)
	     (let ((decls (remove-if #'null (mapcar #'(lambda (s)
							(let ((ts (assoc s alist)))
							  (if (cdr ts)
							      `(type ,(cdr ts) ,s)
							      `(ignore ,s))))
						    syms))))
	       (when decls `((declare ,@decls))))))
    (apply #'recursive-append
	   (append
	    (mapcan #'(lambda (x)
			(destructuring-bind (bind expr type) (let ((tpos (position :type x)) (len (length x)))
							       (list (subseq x 0 (1- (or tpos len))) (nth (1- (or tpos len)) x) (when tpos (nthcdr (1+ tpos) x))))
			  (let* ((typa (loop for (s ty) on (flatten (zip-tree bind type))
					     with skip? = nil
					     if (or skip? (null s)) do (setf skip? nil)
                                             else 
                                             do (progn (setf skip? t)
						       (unless (member s cl:lambda-list-keywords)
							 (collect (cons s ty))))))
			         (vsyms (mapcar #'(lambda (x) (if (consp x)
								  (let ((g (gensym)))
								    (list g
									  `(destructuring-bind (,@x) ,g
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
	    `((progn ,@body))))))

(defmacro lety (bindings &rest body)
  "Like let, but also allows type-declarations with the key :type.

  Example:
  (macroexpand-1
    `(let-typed ((x 1 :type fixnum))
    (+ 1 x)))
  ;; (LET ((X 1))
  ;;   (DECLARE (TYPE FIXNUM X))
  ;;   (+ 1 X))"
  `(let (,@(mapcar #'(lambda (x) (subseq x 0 2)) bindings))
     ,@(let ((types (remove-if #'null (mapcar #'(lambda (x) (destructuring-bind (s e &key (type t)) x
							      (declare (ignore e))
							      (unless (eql type t)
								(if (null type)
								    `(ignore ,s)
								    `(type ,type ,s)))))
					      bindings))))
	 (when types `((declare ,@types))))
     ,@body))

(defmacro lety* (bindings &rest body)
  "Like let*, but also allows type-declarations with the key :type.

Example:
(macroexpand-1
  `(let*-typed ((x 1 :type fixnum))
      (+ 1 x)))
;; (LET* ((X 1))
;;   (DECLARE (TYPE FIXNUM X))
;;   (+ 1 X))"
  `(let* (,@(mapcar #'(lambda (x) (subseq x 0 2)) bindings))
     ,@(let ((types (remove-if #'null
			       (mapcar #'(lambda (x) (destructuring-bind (s e &key (type t)) x
						       (declare (ignore e))
						       (unless (eql type t)
							 (if (null type)
							     `(ignore ,s)
							     `(type ,type ,s)))))
				       bindings))))
	 (when types `((declare ,@types))))
     ,@body))
