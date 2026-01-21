;;; class.lisp --- Tensor Classes

;; 

;;; Code:
(in-package :obj/tensor)

;;Field templates
(define-template-generic (t.f+ #'subtypep) ty (&rest nums))
(define-template-generic (t.f- #'subtypep) ty (&rest nums))
(define-template-generic (t.f* #'subtypep) ty (&rest nums))
(define-template-generic (t.f/ #'subtypep) ty (&rest nums))
(define-template-generic (t.f= #'subtypep) ty (&rest nums))

(macrolet ((def-marith (tname clop)
             `(define-template-method ,tname (ty number) (&rest nums)
                (if (and (consp ty) (eql (first ty) 'mod))
                    `(mod (,',clop ,@(mapcar #'(lambda (x) `(the ,ty ,x)) nums)) ,(second ty))
                    `(,', clop ,@(mapcar #'(lambda (x) `(the ,ty ,x)) nums)))))
           (genarith ((&rest args))
             `(progn ,@(mapcar #'(lambda (x) `(def-marith ,(car x) ,(cadr x))) args))))
  (genarith ((t.f+ cl:+)
             (t.f- cl:-)
             (t.f* cl:*))))

(define-template-method t.f= (ty number) (&rest nums)
  `(cl:= ,@(mapcar #'(lambda (x) `(the ,ty ,x)) nums)))

(define-template-method t.f= (ty t) (&rest nums)
  (let ((zg (zipsym nums)))
    `(let (,@zg)
       (ziprm (and eql) (,@(mapcar #'car zg)) (,@(mapcar #'car (cdr zg)) ,(caar zg))))))

;; chinese remainder theorem solver.
(definline eeuclid (a b)
  (declare (type fixnum a b))
  (let ((ss 0) (s.pr 1)
        (tt 1) (t.pr 0)
        (r b) (r.pr a)
        (tmp 0))
    (declare (type fixnum ss s.pr tt t.pr r r.pr tmp))
    (locally (declare (optimize (speed 3) (safety 0)))
      (loop :while (/= r 0)
         :do (multiple-value-bind (quo rem) (floor r.pr r)
               (declare (type fixnum quo rem))
               (setf r.pr r
                     r rem
                     tmp ss
                     ss (- s.pr (the fixnum (* quo ss)))
                     s.pr tmp
                     tmp tt
                     tt (- t.pr (the fixnum (* quo tt)))
                     t.pr tmp))))
    (values s.pr t.pr r.pr)))

(define-template-method t.f/ (ty number) (&rest nums)
  (if (and (consp ty) (eql (car ty) 'mod))
      (cond
        ((cddr nums) `(t.f/ ,ty ,(car nums) (t.f* ,ty ,@(cdr nums))))
        ((not (cdr nums)) `(t.f/ ,ty (t.fid* ,ty) ,(car nums)))
        (t
         (with-gensyms (s tt g a b)
           `(let ((,a ,(first nums)) (,b ,(second nums)))
              (declare (type ,ty ,a ,b))
              (multiple-value-bind (,s ,tt ,g) (eeuclid ,(second ty) ,b)
                (declare (ignore ,s))
                (if (cl:= ,g (cl:gcd ,a ,g))
                    (t.coerce ,ty (cl:* ,tt (cl:/ ,a ,g)))
                    (error "Cannot solve equation ~a * x = ~a mod ~a" ,a ,b ,(second ty))))))))
      `(cl:/ ,@(mapcar #'(lambda (x) `(the ,ty ,x)) nums))))

(define-template-generic (t.fid+ #'subtypep) ty ())
(define-template-method t.fid+ (ty t) ()
  nil)
(define-template-method t.fid+ (ty number) ()
  (coerce 0 ty))

(define-template-generic (t.fid* #'subtypep) ty ())
(define-template-method t.fid* (ty number) ()
  (coerce 1 ty))

(define-template-generic (t.fc #'subtypep) ty (num))
(define-template-method t.fc (ty number) (num)
 `(cl:conjugate ,num))

(define-template-method t.fc (ty real) (num)
  num)

(defgeneric fc (x)
  (:method ((x complex))
    (cl:conjugate x))
  (:method ((x real))
    x)
  (:method ((x t))
    (let ((clname (class-name (class-of x))))
      (compile-and-eval
       `(defmethod fc ((x ,clname))
          (t.fc ,clname x)))
      (fc x))))

(defun field-realp (fil)
  (eql (macroexpand-1 `(t.fc ,fil phi)) 'phi))

(define-template-generic (t.frealpart #'subtypep) ty (num))
(define-template-method t.frealpart (ty number) (num)
  `(cl:realpart ,num))
(define-template-method t.frealpart (ty real) (num)
  num)

(define-template-generic (t.fimagpart #'subtypep) ty (num))
(define-template-method t.fimagpart (ty number) (num)
  `(cl:imagpart ,num))
(define-template-method t.fimagpart (ty real) (num)
  `(t.fid+ ,ty))

;; (define-template-generic (t.random #'subtypep) ty (num &optional random-state))
;; (define-template-method t.random (sym real) (num &optional random-state)
;;   (if random-state
;;       `(random ,num ,random-state)
;;       `(random ,num)))

(define-template-generic (t.coerce #'subtypep) ty (val))
(define-template-method t.coerce (ty t) (val) val)
(define-template-method t.coerce (ty number) (val)
  (if (and (consp ty) (eql (first ty) 'mod))
      `(mod (coerce ,val 'fixnum) ,(second ty))
      `(coerce ,val ',ty)))

(eval-every
  (defun strict-compare (func-list a b)
    (loop :for func :in func-list
       :for elea :in a
       :for eleb :in b
       :do (unless (funcall func elea eleb)
             (return nil))
       :finally (return t)))

  (defun dict-compare (func-list a b)
    (loop :for func :in func-list
       :for elea :in a
       :for eleb :in b
       :do (when (funcall func elea eleb)
             (return t)))))

;;This one is hard to get one's brain around.
(define-template-generic (t.strict-coerce
               #'(lambda (a b) (strict-compare (list #'subtypep #'(lambda (x y) (subtypep y x))) a b))
               #'(lambda (a b) (dict-compare (list #'subtypep #'subtypep) b a))
               sort)
    (from to) (val))

;;Anything can be coerced into type "t"
(define-template-method t.strict-coerce ((from t) (to t)) (val)
  val)

;;Any number can be coerced into 'double-float (with loss of precision of course)
(define-template-method t.strict-coerce ((from real) (to double-float)) (val)
 `(coerce ,val ',to))

;;-do-
(define-template-method t.strict-coerce ((from real) (to single-float)) (val)
 `(coerce ,val ',to))

;;Any number can be coerced into '(complex double-float) (with loss of precision of course)
(define-template-method t.strict-coerce ((from number) (to (complex double-float))) (val)
 `(coerce ,val ',to))

;;-do-
(define-template-method t.strict-coerce ((from number) (to (complex single-float))) (val)
 `(coerce ,val ',to))

(define-template-method t.strict-coerce ((from rational) (to rational)) (val)
  `(the rational ,val))

(define-template-method t.strict-coerce ((from boolean) (to boolean)) (val)
  `(the boolean ,val))

(define-template-method t.strict-coerce ((from index-type) (to index-type)) (val)
  `(the index-type ,val))
