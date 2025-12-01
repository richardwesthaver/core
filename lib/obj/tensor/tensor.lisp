;;; template.lisp --- Tensor Template

;; 

;;; Code:
(in-package :obj/tensor)

(deft/generic (t.store-type #'subtypep) sym (&optional size))
(eval-every
  (defun store-type (cl &optional (size '*)) (macroexpand-1 `(t.store-type ,cl ,size)))
  (defun store-element-type (x &aux (ftype (field-type x)))
    (or (real-subtypep ftype) ftype))
  (defun linear-storep (cl)
    (typecase cl ((or simple-array simple-bit-vector) t)))
  (defun hash-table-storep (x) (eql (store-type x) 'hash-table))
  (defun clinear-storep (x) (and (subtypep x 'tensor) (linear-storep x) (real-subtypep (field-type x))))
  (defun float-tensorp (type) (member (field-type type) '(single-float double-float (complex single-float) (complex double-float)) :test #'equal)))

                                        ;(class-direct-subclasses (find-class (tensor 'double-float)))
(deft/method t.store-type (type simple-vector-store-mixin) (&optional (size '*))
  `(simple-array ,(or (real-subtypep (field-type type)) (field-type type)) (,size)))
(deft/method t.store-type (type hash-table-store-mixin) (&optional (size '*))
  'hash-table)

(deft/generic (t.store #'subtypep) sym (x))
(deft/method t.store (sym tensor) (x) `(the ,(store-type sym) (slot-value ,x 'store)))

;;tensor specializations
(deft/generic (t.field-type #'subtypep) sym ())
(deft/method t.field-type (sym tensor) () (field-type sym))
(eval-every
  (defun coerceable? (clx cly)
    (handler-case (progn (macroexpand-1 `(t.strict-coerce ((t.field-type ,clx) (t.field-type ,cly)) x)) t)
      (error () nil))))

;;
(deft/generic (t.complexified-tensor #'subtypep) sym ())
(deft/method t.complexified-tensor (class tensor) () (complexified-tensor class))

(deft/generic (t.realified-tensor #'subtypep) sym ())
(deft/method t.realified-tensor (class tensor) () (realified-tensor class))
;;
(deft/generic (t.compute-store-size #'subtypep) sym (size))
(deft/generic (t.store-size #'subtypep) sym (ele))
(deft/generic (t.store-allocator #'subtypep) sym (size &rest initargs))
(deft/generic (t.total-size #'subtypep) sym (ele))
;;
(deft/method t.compute-store-size (cl simple-vector-store-mixin) (size)
  (if (real-subtypep (field-type cl)) `(* 2 ,size) size))

(deft/method (t.store-size #'linear-storep) (sym tensor) (ele)
  (if (clinear-storep sym) `(/ (length ,ele) 2) `(length ,ele)))

(deft/method (t.store-size #'hash-table-storep) (sym stride-accessor) (ele)
  `(hash-table-size ,ele))
;;
(deft/method t.total-size (sym dense-tensor) (ele)
  `(vector-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (* x y))) (the index-store-vector (dimensions ,ele))))

(deft/method (t.total-size #'hash-table-storep) (sym tensor) (ele)
  `(hash-table-count (t.store ,sym ,ele)))

(deft/method t.total-size (sym graph-accessor) (ele)
  `(nth-value 1 (fence ,ele -1)))

(deft/method t.total-size (sym coordinate-accessor) (ele)
  `(slot-value ,ele 'tail))
;;
(deft/method t.store-allocator (sym index-store) (size &rest initargs)
  (letv* ((() initargs))
    `(the index-store (make-array ,size :element-type 'index-type))))
(deft/method t.store-allocator (sym index-store-vector) (size &rest initargs)
  (letv* (((&key initial-element initial-contents) initargs))
    `(the index-store-vector (make-array ,size :element-type 'index-type
                                         ,@(when initial-element `(:initial-element ,initial-element))
                                         ,@(when initial-contents `(:initial-element ,initial-contents))))))
(deft/method t.store-allocator (sym index-store-matrix) (size &rest initargs)
  (letv* ((() initargs))
    `(the index-store-matrix (make-array ,size :element-type 'index-type))))

(deft/method (t.store-allocator #'linear-storep) (sym tensor) (size &rest initargs)
  (letv* (((&key initial-element) initargs))
    (with-gensyms (sitm size-sym arr idx init)
      (let ((type (cond
                    ((and (listp (store-type sym)) (eql (car (store-type sym)) 'simple-array)) (second (store-type sym)))
                    ((and (listp (store-type sym)) (eql (car (store-type sym)) 'simple-bit-vector)) 'bit)
                    (t (error 'invalid-argument :reason "Not a simple array type" :item (store-type sym))))))
        `(let ((,size-sym (t.compute-store-size ,sym (let ((,sitm ,size))
                                                       (etypecase ,sitm
                                                         (index-type ,sitm)
                                                         (index-store-vector (vector-foldr #'* (the index-store-vector ,sitm)))
                                                         (cons (reduce #'* ,sitm))))))
               ,@(when initial-element `((,init ,initial-element :type ,(field-type sym))))
               (,arr (make-array ,size-sym :element-type ',type :initial-element ,(if (subtypep type 'number) `(t.fid+ ,type) nil)) :type ,(store-type sym)))
           ,@(when initial-element
               `((with-optimization (:speed 3 :safety 0) (loop :for ,idx :from 0 :below ,size-sym :do (t.store-set ,sym ,init ,arr ,idx)))))
           ,arr)))))

(deft/method (t.store-allocator #'hash-table-storep) (sym stride-accessor) (size &rest initargs)
  (letv* (((&key size) initargs))
    `(make-hash-table :size ,size)))
;;
(deft/generic (t.store-ref #'subtypep) sym (store &rest idx))
(deft/generic (t.store-set #'subtypep) sym (value store &rest idx))

;; (define-setf-expander t.store-ref (sym store &rest idx &environment env)
;;   (with-gensyms (nval)
;;     (values nil nil `(,nval)
;;             `(t.store-set ,sym ,nval ,store ,@idx)
;;             `(t.store-ref ,sym ,store ,@idx)))
;;   #+nil(multiple-value-bind (dummies vals newval setter getter)
;;       (get-setf-expansion store env)
;;     (declare (ignore newval setter))
;;     (with-gensyms (nval)
;;       (values dummies vals `(,nval)
;;               `(t.store-set ,sym ,nval ,getter ,@idx)
;;               `(t.store-ref ,sym ,getter ,@idx)))))

(define-setf-expander t.store-ref (sym store &rest idx &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion store env)
    (declare (ignore newval setter))
    (with-gensyms (nval)
      (values dummies vals `(,nval)
              `(t.store-set ,sym ,nval ,getter ,@idx)
              `(t.store-ref ,sym ,getter ,@idx)))))

(deft/method t.store-ref (sym simple-vector-store-mixin) (store &rest idx)
  (assert (null (cdr idx)) nil "given more than one index for linear-store")
  (let ((idx (car idx)))
    (if (clinear-storep sym)
        (using-gensyms (decl (store idx) (2idx))
                       `(let (,@decl)
                          (declare (type ,(store-type sym) ,store)
                                   (type index-type ,idx))
                          (let-typed ((,2idx (* 2 ,idx) :type index-type))
                                     (values (complex (aref ,store ,2idx) (aref ,store (1+ ,2idx))) t))))
        `(values (aref (the ,(store-type sym) ,store) (the index-type ,idx)) t))))

(deft/method t.store-set (sym simple-vector-store-mixin) (value store &rest idx)
  (assert (null (cdr idx)) nil "given more than one index for linear-store")
  (let ((idx (car idx)))
    (if (clinear-storep sym)
        (using-gensyms (decl (store idx value) (2idx))
                       `(let (,@decl)
                          (declare (type ,(store-type sym) ,store)
                                   (type ,(field-type sym) ,value)
                                   (type index-type ,idx))
                          (let-typed ((,2idx (* 2 ,idx) :type index-type))
                                     (setf (aref ,store ,2idx) (cl:realpart ,value)
                                           (aref ,store (1+ ,2idx)) (cl:imagpart ,value)))
                          ,value))
        `(setf (aref (the ,(store-type sym) ,store) (the index-type ,idx)) ,value))))
;;
(deft/method (t.store-ref #'hash-table-storep) (sym stride-accessor) (store &rest idx)
  (assert (null (cdr idx)) nil "given more than one index for linear-store")
  `(the (values ,(field-type sym) boolean) (gethash (the index-type ,(car idx)) (the hash-table ,store) (t.fid+ ,(field-type sym)))))

(deft/method (t.store-set #'hash-table-storep) (sym stride-accessor) (value store &rest idx)
  (assert (null (cdr idx)) nil "given more than one index for linear-store")
  (let ((fty (field-type sym))
        (idx (car idx)))
    (using-gensyms (decl (store idx value))
                   `(let (,@decl)
                      (declare (type ,fty ,value))
                      (setf (gethash (the index-type ,idx) (the hash-table ,store)) (the ,(field-type sym) ,value))
                      #+nil(if (t.f= ,fty ,value (t.fid+ ,fty))
                               (progn (remhash ,idx (the hash-table ,store)) (t.fid+ ,fty))
                               )))))

(deft/generic (with-field-element #'subtypep) sym (decl &rest body))
(defmacro with-field-elements (sym decls &rest body)
  (if (null decls) `(progn ,@body)
      `(with-field-element ,sym ,(first decls)
         (with-field-elements ,sym ,(cdr decls) ,@body))))

(deft/method with-field-element (sym tensor) (decl &rest body)
  (destructuring-bind (var init &optional (count 1)) decl
    `(let-typed ((,var (t.store-allocator ,sym ,count :initial-element ,init) :type ,(store-type sym)))
                (locally ,@body))))

;;Blas
(deft/generic (t.blas-lb #'subtypep) sym (i))
(deft/method t.blas-lb (sym blas-mixin) (i)
  (if (clinear-storep sym)
      (ecase i
        (1 '*complex-l1-fcall-lb*)
        (2 '*complex-l2-fcall-lb*)
        (3 '*complex-l3-fcall-lb*))
      (ecase i
        (1 '*real-l1-fcall-lb*)
        (2 '*real-l2-fcall-lb*)
        (3 '*real-l3-fcall-lb*))))


;; (defgeneric testg (x &optional ele))
;; (define-tensor-method testg ((x dense-tensor :a) &optional (ele 1))
;;   `(t/copy! (t ,(cl x)) (coerce ele ',(field-type (cl x))) x)
;;   'x)

;; (testg (zeros 10))

;; (defgeneric copy!-test (x y))

;; (define-tensor-method copy!-test ((x dense-tensor :a) (y dense-tensor :b t))
;;   `(t/copy! (,(cl x) ,(cl y)) x y))

;; (define-tensor-method axpy-test (alpha (x dense-tensor :a) (y dense-tensor :a t))
;;   `(let ((alpha (t/coerce ,(field-type (cl x)) alpha)))
;;      (declare (type ,(field-type (cl x)) alpha))
;;      ,(recursive-append
;;        (when (blas-tensorp (cl x))
;;	 `(if-let ((strd (and (call-fortran? x (t/l1-lb ,(cl x))) (blas-copyablep x y))))
;;	    (t/blas-axpy! ,(cl x) alpha x (first strd) y (second strd))))
;;        `(t/axpy! ,(cl x) alpha x y))))

;; (defgeneric store-ref (tensor idx)
;;   (:documentation  "Generic serial read access to the store.")
;;   (:generic-function-class tensor-method-generator))
;; (defgeneric (setf store-ref) (value tensor idx)
;;   (:generic-function-class tensor-method-generator))
(define-tensor-method store-ref ((tensor tensor :x) idx)
  `(t.store-ref ,(cl :x) (t.store ,(cl :x) tensor) idx))
(define-tensor-method (setf store-ref) (value (tensor tensor :x) idx)
  `(t.store-set ,(cl :x) (t.coerce ,(field-type (cl :x)) value) (t.store ,(cl :x) tensor) idx))

(defgeneric ref (tensor &rest subscripts)
  (:documentation "
  Syntax
  ======
  (ref store subscripts)

  Purpose
  =======
  Return the element corresponding to subscripts.
")
  (:generic-function-class tensor-method-generator))

(defgeneric (setf ref) (value tensor &rest subscripts)
  (:generic-function-class tensor-method-generator))
;;
;; (defgeneric store-size (obj)
;;   (:documentation "(store-size obj) => store-size

;;   Returns the number of elements the store of the obj can hold. This is not
;;   necessarily equal to that returned by total-size.")
;;   (:generic-function-class tensor-method-generator))

(define-tensor-method store-size ((tensor tensor :x))
  `(t.store-size ,(cl :x) (slot-value tensor 'store)))

;; (defgeneric total-size (obj)
;;   (:method ((obj sequence)) (length obj))
;;   (:method ((arr array)) (array-total-size arr))
;;   (:generic-function-class tensor-method-generator))
(define-tensor-method total-size ((obj tensor :x))
  `(t.total-size ,(cl :x) obj))
(defmethod total-size ((x dense-tensor))
  (t.total-size dense-tensor x))
(defmethod total-size ((x sequence))
  (length x))
(defmethod total-size ((x array))
  (array-total-size x))
                       
