;;; sym.lisp --- Math Symbols

;; Math Symbol Definitions

;;; Code:
(in-package :math/sym)
(sb-ext:unlock-package :cl)
;;; Arithmetic
;;All the b macros will (only) assume that the first argument is destructible
;;when both arguments are supplied (i.e (not (null b))).

(defmacro b+ (a &optional b)
  (using-gensyms (decl (a b))
    `(let (,@decl)
       (cart-etypecase (,a ,b)
         ((number number) (cl:+ ,a ,b))
         ((number null) ,a)
         ((tensor (or tensor number)) (axpy! 1 ,b ,a))
         ((number tensor) (axpy 1 ,a ,b))
         ((tensor null) (tensor-copy ,a))))))

(definline + (&rest objects &aux (ret (if objects (b+ (car objects)) 0)))
  (loop for fst in (cdr objects) do (setf ret (b+ ret fst)))
  ret)

(define-compiler-macro + (&rest objects)
  (rec reducer ((ret (if objects `(b+ ,(car objects)) 0)) (objs (cdr objects)))
       (if (not objs) ret
           (reducer `(b+ ,ret ,(car objs)) (cdr objs)))))

#+nil
(defun test+ ()
  (let ((a (print (if (cl:= (random 2) 0) 1 (randn 1)))))
    (time
     (etypecase a
       (tensor (dotimes (i 100000) (+ a a)))
       (number (dotimes (i 100000) (+ a a)))))))
;;
(defmacro b- (a &optional b)  
  (using-gensyms (decl (a b))
    `(let (,@decl)
       (cart-etypecase (,a ,b)
         ((number number) (cl:- ,a ,b))
         ((number null) (cl:- ,a))	 
         ((tensor (or tensor number)) (axpy! -1 ,b ,a))
         ((number tensor) (b+ (scal -1 ,b) ,a))
         ((tensor null) (scal -1 ,a))))))

(definline - (object &rest more-objects)
  (if more-objects
      (let ((ret (b+ object)))
        (loop for fst in more-objects do (setf ret (b- ret fst)))
        ret)
      (b- object)))

(define-compiler-macro - (object &rest more-objects)
  (if more-objects
      (rec reducer ((ret `(b+ ,object)) (objs more-objects))
           (if (not objs) ret
               (reducer `(b- ,ret ,(car objs)) (cdr objs))))
      `(b- ,object)))

#+nil
(defun test- ()
  (let ((a (print (if (cl:= (random 2) 0) 1 (randn 1)))))
    (time
     (etypecase a
       (tensor (dotimes (i 100000) (- a a a)))
       (number (dotimes (i 100000) (- a a a)))))))
;;
(defmacro b.* (a &optional b)
  (using-gensyms (decl (a b))
    `(let (,@decl)
       (cart-etypecase (,a ,b)
         ((number number) (cl:* ,a ,b))
         ((number null) ,a)
         ((tensor (or tensor number)) (scal! ,b ,a))
         ((number tensor) (scal ,a ,b))
         ((tensor null) (tensor-copy ,a))))))

(definline .* (&rest objects &aux (ret (if objects (b.* (car objects)) 1)))
  (loop for fst in (cdr objects) do (setf ret (b.* ret fst)))
  ret)

(define-compiler-macro .* (&rest objects)
  (rec reducer ((ret (if objects `(b.* ,(car objects)) 1)) (objs (cdr objects)))
       (if (not objs) ret
           (reducer `(b.* ,ret ,(car objs)) (cdr objs)))))
;;
(defmacro b* (a &optional b)
  (match* (a b)
    (((or (list (and op-a (or 'ctranspose 'ctranspose~ 'transpose 'transpose~)) code-a) code-a)
      (or (list (and op-b (or 'ctranspose 'ctranspose~ 'transpose 'transpose~)) code-b) code-b))
     (using-gensyms (decl (code-a code-b))
       (symbol-macrolet ((a (if op-a `(,op-a ,code-a) code-a))
                         (b (if op-b `(,op-b ,code-b) code-b)))
         `(let (,@decl)
            (cart-etypecase (,code-a ,code-b)
              ((number number) (cl:* ,a ,b))
              ((number null) ,a)
              ;;Scaling
              ((number tensor) (scal ,a ,b))
              ((tensor number) (scal ,b ,a))
              ;;Matrix, vector/matrix product
              ((tensor-matrix (or tensor-matrix tensor-vector)) (gem 1 ,a ,b nil nil))
              ((tensor null) (tensor-copy ,a))
              ;;Permutation action. Left action permutes axis-0, right action permutes the last axis (-1).
              ((permutation base-tensor) (permute ,b ,a 0))
              ((tensor permutation) (permute ,a ,b -1))
              ;;The correctness of this depends on the left-right order in reduce (foldl).
              ((permutation permutation) (permutation* ,a ,b))
              ((permutation null) (tensor-copy ,a)))))))))

(definline * (&rest objects &aux (ret (if objects (b* (car objects)) 1)))
  (loop for fst in (cdr objects) do (setf ret (b* ret fst)))
  ret)

(define-compiler-macro * (&rest objects)
  (rec reducer ((ret (if objects `(b* ,(car objects)) 1)) (objs (cdr objects)))
       (if (not objs) ret	  
           (destructuring-case (car objs)
             (('/ a)
              (if (cdr objs)
                  `(b* ,ret (b\\ ,@(if (cddr objs) `((* ,@(cdr objs))) (cdr objs)) ,a))
                  `(b/ ,ret ,a)))
             ((t) (reducer `(b* ,ret ,(car objs)) (cdr objs)))))))

(defmacro b./ (a &optional b)
  (using-gensyms (decl (a b))
    `(let (,@decl)
       (cart-etypecase (,a ,b)
         ((number number) (cl:/ ,a ,b))
         ((number null) (cl:/ ,a))
         ((tensor (or tensor number)) (div! ,b ,a))
         ((number tensor) (div ,b ,a))
         ((tensor null) (div ,a 1))))))

(definline ./ (object &rest more-objects)
  (if more-objects
      (let ((ret (b+ object)))
        (loop for fst in more-objects do (setf ret (b./ ret fst)))
        ret)
      (b./ object)))

(define-compiler-macro ./ (object &rest more-objects)
  (if more-objects
      (rec reducer ((ret `(b+ ,object)) (objs more-objects))
           (if (not objs) ret
               (reducer `(b./ ,ret ,(car objs)) (cdr objs))))
      `(b./ ,object)))
;;
(defmacro b@ (a &optional b)
  (match* (a b)
    (((or (list (and op-a (or 'ctranspose 'ctranspose~ 'transpose 'transpose~)) code-a) code-a)
      (or (list (and op-b (or 'ctranspose 'ctranspose~ 'transpose 'transpose~)) code-b) code-b))
     (using-gensyms (decl (code-a code-b))
       (symbol-macrolet ((a (if op-a `(,op-a ,code-a) code-a))
                         (b (if op-b `(,op-b ,code-b) code-b)))
         `(let (,@decl)
            (cart-etypecase (,code-a ,code-b)
              ((number number) (cl:* ,a ,b))
              ((number null) ,a)
              ;;Scaling
              ((number tensor) (scal ,a ,b))
              ((tensor number) (scal ,b ,a))
              ;;Matrix, vector/matrix product
              ((tensor-vector tensor-vector) (dot ,a ,b nil))
              ((tensor-matrix (or tensor-matrix tensor-vector)) (gem 1 ,a ,b nil nil))
              ((tensor-vector tensor-matrix) (gem 1 ,b ,a nil nil :t))
              ((tensor tensor) (gett! 1 ,a ,b 1 (zeros (append (butlast (dimensions ,code-a t)) (cdr (dimensions ,code-b t))) (class-of ,code-a))))
              ((tensor null) (tensor-copy ,a))
              ;;Permutation action on arguments. Left action unpermutes arguments, right action permutes them.
              ;;See tb* for comparison.
              ((permutation tensor) (transpose ,b (permutation/ ,a)))
              ((tensor permutation) (transpose! ,a ,b))
              ;;The correctness of this depends on the left-right order in reduce (foldl).
              ((permutation permutation) (permutation* ,a ,b))
              ((permutation null) (tensor-copy ,a)))))))))

(definline @ (&rest objects &aux (ret (if objects (b@ (car objects)) 1)))
    (loop for fst in (cdr objects) do (setf ret (b@ ret fst)))
  ret)

(define-compiler-macro @ (&rest objects)
  (rec reducer ((ret (if objects `(b@ ,(car objects)) 1)) (objs (cdr objects)))
       (if (not objs) ret
           (reducer `(b@ ,ret ,(car objs)) (cdr objs)))))

(definline · (&rest objects) (apply #'@ objects))
#+nil
(defun test· ()
  (let ((a (print (if (cl:= (random 2) 0) 1 (randn 1)))))
    (time (dotimes (i 100000) (· a a)))
    #+nil
    (time
     (etypecase a
       (tensor (dotimes (i 100000) (+ a a)))
       (number (dotimes (i 100000) (+ a a)))))))
;;
(defmacro b/ (a &optional b)
  "Solve x b = a (a /b); or compute /a"
  (using-gensyms (decl (a b))
    `(let (,@decl)
       (cart-etypecase (,a ,b)
         ((number number) (cl:/ ,a ,b))
         ((number null) (cl:/ ,a))
         ((tensor number) (div! ,b ,a))
         (((or tensor-vector tensor-matrix) tensor-square-matrix) ;; (/b' a')' =  a / b	  
          (copy! (transpose!
                  (let ((tensor:*default-stride-ordering* :col-major))
                    (getrs! (getrf! (tensor-copy ,b)) (transpose ,a) :t)))
                 ,a))
         ((tensor-square-matrix null) (getri! (getrf! (tensor-copy ,a))))
         ((tensor permutation) (permute! ,a (permutation/ ,b) -1))
         ;;The correctness of this depends on the left-right order in reduce (foldl).
         ((permutation permutation) (permutation* ,a (permutation/ ,b)))
         ((permutation null) (permutation/ ,a))))))

(defmacro b\\ (a &optional b)
  "Solve b x = a (/b a); or compute /a"
  (using-gensyms (decl (a b))
    `(let (,@decl)
       (cart-etypecase (,a ,b)
         ((number number) (cl:/ ,a ,b))
         ((number null) (cl:/ ,a))
         ((tensor number) (div! ,b ,a))
         (((or tensor-vector tensor-matrix) tensor-square-matrix)
          (let ((tensor:*default-stride-ordering* :col-major))
            (getrs! (getrf! (tensor-copy ,b)) ,a)))
         ((tensor-square-matrix null) (getri! (getrf! (tensor-copy ,a))))
         ((tensor permutation) (permute! ,a (permutation/ ,b) 0))
         ;;The correctness of this depends on the left-right order in reduce (foldl).
         ((permutation permutation) (permutation* (permutation/ ,b) ,a))
         ((permutation null) (permutation/ ,a))))))

(definline / (object &rest more-objects)
  (if more-objects
      (let ((ret (b+ object)))
        (loop for fst in more-objects do (setf ret (b/ ret fst)))
        ret)
      (b/ object)))

(define-compiler-macro / (object &rest more-objects)
  (if more-objects
      (rec reducer ((ret `(b+ ,object)) (objs more-objects))
           (if (not objs) ret
               (reducer `(b/ ,ret ,(car objs)) (cdr objs))))
      `(b/ ,object)))
;;
(defmacro b= (a &optional b)
  (using-gensyms (decl (a b))
    `(let (,@decl)
       (cart-etypecase (,a ,b)
         ((number number) (cl:= ,a ,b))	 
         (((or tensor number) tensor) (ga= ,a ,b))
         ((tensor number) (ga= ,b ,a))
         (((or number tensor) null) t)))))

(definline = (object &rest more-objects)
  (loop for fst in more-objects
        unless (b= object fst) do (return nil)
        finally (return t)))

(define-compiler-macro = (object &rest more-objects)
  (with-gensyms (o)
    `(let ((,o ,object))
       (and ,@ (mapcar #'(lambda (x) `(b= ,o ,x)) more-objects)))))
;;
;; (defmacro b.= (a &optional b)
;;   (using-gensyms (decl (a b))
;;     `(let (,@decl)
;;        (cart-etypecase (,a ,b)
;; 	 ((number number) (cl:= ,a ,b))	 
;; 	 (((or tensor number) tensor) (ge= ,a ,b))
;; 	 ((tensor number) (ge= ,b ,a))
;; 	 (((or number tensor) null) t)))))

;; (definline .= (object &rest more-objects)
;;   (iter( (for fst in more-objects)
;; 	(unless (b.= object fst) (return nil))
;; 	(finally (return t))))

;; (define-compiler-macro .= (object &rest more-objects)
;;   (with-gensyms (o)
;;     `(let ((,o ,object))
;;        (and ,@ (mapcar #'(lambda (x) `(b.= ,o ,x)) more-objects)))))
;;
(defmacro b⊗ (a &optional b)
  (using-gensyms (decl (a b))
    `(let (,@decl)
       (cart-etypecase (,a ,b)
         ((number number) (cl:* ,a ,b))
         ((number null) ,a)
         ((tensor number) (orphanize (suptensor~ (scal! ,b ,a) (1+ (order ,a)))))
         ((number tensor) (orphanize (suptensor~ (scal ,a ,b) (1+ (order ,b)) 1)))
         ((tensor-vector tensor-vector) (ger 1 ,a ,b nil nil))
         ((tensor tensor) (gekr! 1 ,a ,b 1 (zeros (append (dimensions ,a t) (dimensions ,b t)) (class-of ,a))))
         ((tensor null) (b+ ,a))))))

(definline ⊗ (object &rest more-objects &aux (ret (b⊗ object)))
  (loop for fst in more-objects do (setf ret (b⊗ ret fst)))
  ret)

(define-compiler-macro ⊗ (object &rest more-objects)
  (rec reducer ((ret `(b⊗ ,object)) (objs more-objects))
       (if (not objs) ret
           (reducer `(b⊗ ,ret ,(car objs)) (cdr objs)))))
;;
(definline realpart~ (object) (tensor-realpart~ object))
(definline realpart (object) (tensor-realpart object))
(definline imagpart~ (object) (tensor-imagpart~ object))
(definline imagpart (object) (tensor-imagpart object))
(definline sum! (x y &optional axis) (tensor-sum! x y axis))
(definline sum (x &optional axis preserve-rankp) (tensor-sum x axis preserve-rankp))

;;(tensor-max )

;;; Functions
;;conjugate
(definline conjugate! (a)
  (tensor-conjugate! a))

(definline conjugate (a)
  (tensor-conjugate a))
;;

(defmacro lift-function (fn &aux (pkg (find-package "MATH-USER")))
  (letv* ((fname (symbol-name fn)) (fpkg (symbol-package fn)))
    (letv* ((fn (find-symbol fname fpkg))
            (fn-package (intern fname pkg))
            (ge-fn (intern (concatenate 'string fname "-GENERIC!") pkg)))
      `(progn
         (defgeneric ,ge-fn (x)
           (:generic-function-class tensor-method-generator))
         (define-tensor-method ,ge-fn ((x dense-tensor :x))
           `(dorefs (idx (dimensions x))
                    ((ref-x x :type ,(tensor:cl :x)))
                    (setf ref-x (,',fn ref-x)))
           'x)
         (definline ,(intern (concatenate 'string fname "!") (find-package "MATH-USER")) (x)
           (etypecase x
             (number (,fn x))
             (tensor (,ge-fn x))))
         (definline ,fn-package (x)
           (etypecase x
             (number (,fn x))
             (tensor (,ge-fn (tensor-copy x)))))))))

(macrolet ((lift-fns (&rest lst)
             `(progn ,@ (mapcar #'(lambda (x) `(lift-function ,x)) lst))))
  (lift-fns cl:sin cl:cos cl:tan cl:asin cl:acos cl:exp cl:sinh cl:cosh cl:tanh cl:asinh cl:acosh cl:atanh))

;;log
(defgeneric log-generic! (x y)
  (:generic-function-class tensor-method-generator))
(define-tensor-method log-generic! ((x dense-tensor :x) (y dense-tensor :y))
  `(dorefs (idx (dimensions x))
           ((ref-x x :type ,(tensor:cl :x))
            (ref-y y :type ,(tensor:cl :y)))
     (setf ref-x (cl:log ref-x ref-y)))
  'x)
(define-tensor-method log-generic! ((x dense-tensor :x) (y number))
  `(dorefs (idx (dimensions x))
           ((ref-x x :type ,(tensor:cl :x)))
     (setf ref-x (cl:log ref-x y)))
  'x)
(define-tensor-method log-generic! ((x dense-tensor :x) (y null))
  `(dorefs (idx (dimensions x))
           ((ref-x x :type ,(tensor:cl :x)))
     (setf ref-x (cl:log ref-x)))
  'x)

(definline log! (base &optional power)
  (cart-etypecase (base power)
    ((number number) (cl:log base power))
    ((tensor (or tensor number)) (log-generic! base power))))
(definline log (base &optional power)
  (cart-etypecase (base power)
    ((number number) (cl:log base power))
    ((number null) (cl:log base))
    ((tensor (or tensor number null)) (log-generic! (tensor-copy base (complexified-tensor (class-of base))) power))
    ((number tensor) (log-generic! (copy! base (zeros (dimensions power) (tensor (let ((type (type-of base)))
                                                                                   (if (subtypep type 'complex) type `(complex ,type))))))
                                   power))))
;;atan
(defgeneric atan-generic! (x y)
  (:generic-function-class tensor-method-generator))
(define-tensor-method atan-generic! ((x dense-tensor :x) (y dense-tensor :y))
  `(dorefs (idx (dimensions x))
           ((ref-x x :type ,(tensor:cl :x))
            (ref-y y :type ,(tensor:cl :y)))
     (setf ref-x (cl:atan ref-x ref-y)))
  'x)
(define-tensor-method atan-generic! ((x dense-tensor :x) (y number))
  `(dorefs (idx (dimensions x))
           ((ref-x x :type ,(tensor:cl :x)))
     (setf ref-x (cl:atan ref-x y)))
  'x)
(define-tensor-method atan-generic! ((x dense-tensor :x) (y null))
  `(dorefs (idx (dimensions x))
           ((ref-x x :type ,(tensor:cl :x)))
     (setf ref-x (cl:atan ref-x)))
  'x)

(definline atan! (y &optional x)
  (cart-etypecase (y x)
    ((number number) (cl:atan y x))
    ((number null) (cl:atan y))
    ((tensor (or tensor number)) (atan-generic! y x))))
(definline atan (y &optional x)
  (cart-etypecase (y x)
    ((number number) (cl:atan y x))
    ((number null) (cl:atan y))
    ((tensor (or tensor number null)) (atan-generic! (tensor-copy y (complexified-tensor (class-of y))) x))
    ((number tensor) (atan-generic! (copy! y (zeros (dimensions x) (tensor (let ((type (type-of y)))
                                                                             (if (subtypep type 'complex) type `(complex ,type))))))
                                    x))))
;;expt
(defgeneric expt-generic! (x y)
  (:generic-function-class tensor-method-generator))
(define-tensor-method expt-generic! ((x dense-tensor :x) (y dense-tensor :y))
  `(dorefs (idx (dimensions x))
           ((ref-x x :type ,(tensor:cl :x))
            (ref-y y :type ,(tensor:cl :y)))
           (setf ref-x (expt ref-x ref-y)))
  'x)
(define-tensor-method expt-generic! ((x dense-tensor :x) (y number))
  `(dorefs (idx (dimensions x))
           ((ref-x x :type ,(tensor:cl :x)))
     (setf ref-x (expt ref-x y)))
  'x)

(definline expt! (base power)
  (cart-etypecase (base power)
    ((number number) (cl:expt base power))
    ((tensor (or tensor number)) (expt-generic! base power))))
(definline expt (base power)
  (cart-etypecase (base power)
    ((number number) (cl:expt base power))
    ((tensor (or tensor number)) (expt-generic! (tensor-copy base) power))
    ((number tensor) (expt-generic! (copy! base (zeros (dimensions power) (tensor (type-of base)))) power))))

(sb-ext:lock-package :cl)
