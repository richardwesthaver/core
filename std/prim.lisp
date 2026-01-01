;;; prim.lisp --- Primitive Macros

;; 

;;; Code:
(in-package :std/prim)

;;; EARLY MACROS
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun g!-symbol-p (s)
    "Return T if S is a G!-symbol (gensym'd)."
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
		  "G!"
		  :start1 0
		  :end1 2)))

  (defun o!-symbol-p (s)
    "Return T if S is a O!-symbol (oneshot)."
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
		  "O!"
		  :start1 0
		  :end1 2)))

  (defun o!-symbol-to-g!-symbol (s)
    "Convert O!-symbol S to a G!-symbol."
    (symb "G!" (subseq (symbol-name s) 2))))

(defmacro defmacro/g! (name args &body body)
  "Define a macro with G!-symbols in ARGS automatically converted to gensyms."
  (let ((syms (remove-duplicates
	       (remove-if-not #'g!-symbol-p
			      (flatten* body)))))
    (multiple-value-bind (body declarations docstring)
	(parse-body body :documentation t)
      `(defmacro ,name ,args
	 ,@(when docstring
	     (list docstring))
	 ,@declarations
	 (let ,(mapcar
		(lambda (s)
		  `(,s (gensym ,(subseq
				 (symbol-name s)
				 2))))
		syms)
	   ,@body)))))

(defmacro defmacro! (name args &body body)
  "Define a macro with G!-symbols in ARGS converted to gensyms and O!-symbols
evaluated once and bound to a G!-symbol for use in BODY."
  (let* ((os (remove-if-not #'o!-symbol-p (flatten* args)))
	 (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    (multiple-value-bind (body declarations docstring)
	(parse-body body :documentation t)
      `(defmacro/g! ,name ,args
	 ,@(when docstring
	     (list docstring))
	 ,@declarations
	 `(let ,(mapcar #'list (list ,@gs) (list ,@os))
	    ,(progn ,@body))))))

(defmacro defun! (name args &body body)
  "Define a function with G!-symbols in ARGS automatically converted."
  (let ((syms (remove-duplicates
	       (remove-if-not #'g!-symbol-p
			      (flatten* body)))))
    (multiple-value-bind (body declarations docstring)
	(parse-body body :documentation t)
      `(defun ,name ,args
	 ,@(when docstring
	     (list docstring))
	 ,@declarations
	 (let ,(mapcar (lambda (s)
			 `(,s (gensym ,(subseq (symbol-name s)
					       2))))
		syms)
	   ,@body)))))

;;; Util
(defmacro pswap (a b)
  "Swap the values of A and B using PSETF."
  `(psetf ,a ,b
          ,b ,a))
(defun unquote-args (lst args)
  "Makes a list suitable for use inside macros (sort-of), by building a
new list quoting every symbol in @arg{lst} other than those in @arg{args}.
CAUTION: DO NOT use backquotes!

@lisp
Example:
> (unquote-args '(+ x y z) '(x y))
=> (LIST '+ X Y 'Z)

> (unquote-args '(let ((x 1)) (+ x 1)) '(x))
=> (LIST 'LET (LIST (LIST X '1)) (LIST '+ X '1))
@end lisp"
  (maptree-if #'(lambda (x) (or (symbolp x) (consp x)))
              #'(lambda (x) (etypecase x
                              (symbol (if (member x args) x `(quote ,x)))
                              (cons (values `(list ,@x) #'(lambda (f x) (cons (first x) (mapcar f (cdr x))))))))
              lst))

(defmacro definline (name lambda-list &body body)
  "Define an inlined function."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list ,@body)))

(defmacro defnotinline (name lambda-list &body body)
  `(progn
     (declaim (notinline ,name))
     (defun ,name ,lambda-list ,@body)))

(defmacro with-optimization ((&rest args) &body body)
  "Create a local environment with optimization declarations ARGS and execute
BODY.

Example:
(macroexpand-1
  `(with-optimization (:speed 2 :safety 3)
  (+ 1d0 2d0)))
;; (LOCALLY (DECLARE (OPTIMIZE (SPEED 2) (SAFETY 3))) (+ 1.0d0 2.0d0))"
  `(locally
       ,(recursive-append
	 `(declare (optimize ,@(multiple-value-call #'mapcar #'(lambda (key val) (list (intern (symbol-name key)) val))
						    (loop :for ele :in args
						       :counting t :into cnt
						       :if (oddp cnt)
							 :collect ele into key
						       :else
							 :collect (progn (assert (member ele '(0 1 2 3))) ele) into val
						       :finally (return (values key val))))))
	 (when (and (consp (car body)) (eq (caar body) 'declare))
	   (cdar body)))
     ,@(if (and (consp (car body)) (eq (caar body) 'declare)) (cdr body) body)))

(defmacro macrofy (lambda-func)
  "Returns a MACRO-FUNCTION-like function which can be called later for use
inside macros.

DO NOT USE backquotes in the lambda function!

Example:
(macroexpand-1 `(macrofy (lambda (x y z) (+ (sin x) y (apply #'cos (list z))))))
;; (LAMBDA (X Y Z)
;;   (LIST '+ (LIST 'SIN X) Y (LIST 'APPLY (LIST 'FUNCTION 'COS) (LIST 'LIST Z))))
;; T

(funcall (macrofy (lambda (x y z) (+ (sin x) y (apply #'cos (list z))))) 'a 'b 'c)
;; (+ (SIN A) B (APPLY #'COS (LIST C)))"
  (destructuring-bind (labd args &rest body) lambda-func
    (assert (eq labd 'lambda))
    `(lambda ,args ,@(cdr (unquote-args body args)))))

(defmacro with-marking (&rest body)
  "This macro basically declares local-variables globally,
 while keeping semantics and scope local.

Example:
(macroexpand-1
  `(with-marking
       (loop :for i := 0 :then (1+ i)
          :do (mark* ((xi (* 10 2) :type index-type)
    		  (sum 0 :type index-type))
    		 (incf sum (mark (* 10 2)))
    		 (if (= i 10)
    		     (return sum))))))

; (LET* ((#:G1083 (* 10 2)) (#:SUM1082 0) (#:XI1081 (* 10 2)))
;   (DECLARE (TYPE INDEX-TYPE #:SUM1082)
;   	 (TYPE INDEX-TYPE #:XI1081))
;   (LOOP :FOR I := 0 :THEN (1+ I)
;         :DO (SYMBOL-MACROLET ((XI #:XI1081) (SUM #:SUM1082))
;   	    (INCF SUM #:G1083)
;   	    (IF (= I 10)
;   		(RETURN SUM)))))
; T"
  (let* ((decls nil)
	 (types nil)
	 (code (maptree '(:mark* :mark :memo)
			#'(lambda (mrk)
			    (ecase (car mrk)
			      (:mark*
			       `(symbol-macrolet (,@(mapcar #'(lambda (decl) (destructuring-bind (ref code &key type) decl
									       (let ((rsym (gensym (symbol-name ref))))
										 (push `(,rsym ,code) decls)
										 (when type
										   (push `(type ,type ,rsym) types))
										 `(,ref ,rsym))))
							    (cadr mrk)))
				  ,@(cddr mrk)))
			      (:mark
			       (destructuring-bind (code &key type) (cdr mrk)
				 (let ((rsym (gensym)))
				   (push `(,rsym ,code) decls)
				   (when type
				     (push `(type ,type ,rsym) types))
				   rsym)))
			      (:memo
			       (destructuring-bind (code &key type) (cdr mrk)
				 (let ((memo (find code decls :key #'cadr :test #'tree-equal)))
				   (if memo
				       (car memo)
				       (let ((rsym (gensym)))
					 (push `(,rsym ,code) decls)
					 (when type
					   (push `(type ,type ,rsym) types))
					 rsym)))))))
			body)))
    `(let* (,@decls)
       ,@(when types `((declare ,@types)))
       ,@code)))

;;; Gensyms
(defmacro using-gensyms ((decl (&rest syms) &optional gensyms) &rest body)
  "Bind DECL to a list of let-bindings where a fresh gensym is bound to the
corresponding value in SYMS based on its symbol-name. GENSYMS is an optional
list of additional unbound gensyms."
  `(let ((,decl (zip-list ',(mapcar #'(lambda (x) (gensym (symbol-name x))) syms) (list ,@syms))))
     (destructuring-bind (,@syms) (mapcar #'car ,decl)
       ,(append
         (if gensyms
           `(with-gensyms (,@gensyms)) `(progn))
         body))))

(defmacro binding-gensyms ((mname &optional (fname (gensym))) &rest body)
  "Bind MNAME to a macro and FNAME to a function which dynamically inserts gensyms based on
the argument given, which should be a symbol."
  (with-gensyms (htbl)
    `(let ((,htbl (make-hash-table)))
       (labels ((,fname (x) (or (gethash x ,htbl) (setf (gethash x ,htbl) (gensym (symbol-name x))))))
         (macrolet ((,mname (x) `(,', fname ',x)))
           ,@body)))))

;;; Safe IO Syntax
(defvar *standard-readtable* (with-standard-io-syntax *readtable*)
  "The standard readtable, implementing the syntax specified by the CLHS.
It must never be modified, though only good implementations will even enforce that.")

(defmacro with-safe-io-syntax ((&optional (package :std-user)) &body body)
  "Establish safe CL reader options around the evaluation of BODY"
  `(call-with-safe-io-syntax #'(lambda () (let ((*package* (find-package ,package))) ,@body))))

(defun call-with-safe-io-syntax (thunk &key (package :std-user))
  "Call THUNK with safe CL reader options."
  (with-standard-io-syntax
    (let ((*package* (find-package package))
          (*read-default-float-format* 'double-float)
          (*print-readably* nil)
          (*read-eval* nil))
      (funcall thunk))))

(defun safe-read-from-string (string &key (package :cl) (eof-error-p t) eof-value (start 0) end preserve-whitespace)
  "Read from STRING using a safe syntax, as per WITH-SAFE-IO-SYNTAX"
  (with-safe-io-syntax (package)
    (read-from-string string eof-error-p eof-value :start start :end end :preserve-whitespace preserve-whitespace)))

(definline read-until-end (stream)
  "Read input from STREAM until EOF and return a string."
  (with-output-to-string (s)
    (loop for c = (read-char stream nil)
          until (not c)
          do (write-char c s))))

(definline read-lisp-until-end (stream)
  "Read input from STREAM until EOF and return a form."
  (with-gensyms (eof)
    (loop for c = (read stream nil eof)
          until (eql c eof)
          collect c)))

(defun read-lisp-file (file &key if-does-not-exist (external-format :default))
  "Read all forms in a lisp FILE."
  (with-open-file (f file :if-does-not-exist if-does-not-exist :external-format external-format)
    (read-lisp-until-end f)))
