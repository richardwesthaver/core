;;; tensor.lisp --- CUDA Tensors

;; 

;;; Code:
(in-package :math/cuda)

(defclass cuda-vector-store-mixin () ()
  (:documentation "Mixin which indicates that this type supports CUDA alien routines."))

(define-template-method t.store-type (type cuda-vector-store-mixin) (&optional (size '*))
  (cuda-vector (or (real-subtypep (field-type type)) (field-type type))))
(define-template-method t.compute-store-size (cl cuda-vector-store-mixin) (size)
  (if (real-subtypep (field-type cl)) `(* 2 ,size) size))
(define-template-method t.store-size (cl cuda-vector-store-mixin) (vec)
  (if (real-subtypep (field-type cl)) `(/ (slot-value (the ,(store-type cl) ,vec) 'length) 2) `(slot-value (the ,(store-type cl) ,vec) 'length)))

(define-template-method t.store-ref (class cuda-vector-store-mixin) (store &rest idx)
  (assert (null (cdr idx)) nil "given more than one index for linear-store")
  (let ((idx (car idx)))
    (if (real-subtypep (field-type class))
        (using-gensyms (decl (store idx) (2idx))
          `(let (,@decl)
             (declare (type ,(store-type class) ,store)
                      (type index-type ,idx))
             (lety ((,2idx (* 2 ,idx) :type index-type))
               (values (complex (fvref (the ,(store-type class) ,store) ,2idx) (fvref (the ,(store-type class) ,store) (1+ ,2idx))) t))))
        `(values (fvref (the ,(store-type class) ,store) (the index-type ,idx)) t))))

(define-template-method t.store-set (class cuda-vector-store-mixin) (value store &rest idx)
  (assert (null (cdr idx)) nil "given more than one index for linear-store")
  (let ((idx (car idx)))
    (if-let ((real-type (real-subtypep (field-type class))))
        (using-gensyms (decl (store idx value) (2idx))
          `(let (,@decl)
             (declare (type ,(store-type class) ,store)
                      (type ,(field-type class) ,value)
                      (type index-type ,idx))
             (lety ((,2idx (* 2 ,idx) :type index-type))
               (funcall #'(setf fvref) (the ,real-type (cl:realpart ,value)) (the ,(store-type class) ,store) ,2idx)
               (funcall #'(setf fvref) (the ,real-type (cl:imagpart ,value)) (the ,(store-type class) ,store) (1+ ,2idx)))
             ,value))
        `(funcall #'(setf fvref) (the ,(field-type class) ,value) (the ,(store-type class) ,store) (the index-type ,idx)))))

(define-template-method t.store-allocator (type cuda-vector-store-mixin) (size &rest initargs)
  (letv* (((&key (initial-element (coerce 0 (field-type type)))) initargs)
          (element-type (or (real-subtypep (field-type type)) (field-type type))))
    (with-gensyms (sitm len vec idx init sap)
      `(lety* ((,len (t.compute-store-size ,type (let ((,sitm ,size))
                                                   (etypecase ,sitm
                                                     (index-type ,sitm)
                                                     (index-store-vector (vector-foldr #'* (the index-store-vector ,sitm)))
                                                     (cons (reduce #'* ,sitm))))))
               ,@(when initial-element `((,init ,initial-element :type ,(field-type type))))
               (,vec (let* ((,sap (foreign-alloc ',(element-type-to-alien element-type) :count ,len))
                            (,vec (make-instance (cuda-vector ',element-type) :sap ,sap :length ,len)))
                       (sb-ext:finalize ,vec #'(lambda () (foreign-free ,sap)))
                       ,vec)))
         ,@(when initial-element
             `((with-optimization (:speed 3 :safety 0)
                 (loop :for ,idx :from 0 :below (t.store-size ,type ,vec)
                       :do (setf (t.store-ref ,type (the ,(cuda-vector element-type) ,vec) ,idx) (the ,(field-type type) ,init))))))
         ,vec))))

(define-template-method with-field-element (cl cuda-vector-store-mixin) (decl &rest body)
  (destructuring-bind (var init &optional (count 1)) decl
    (with-gensyms (idx size point init_)
      (let ((type (element-type (store-type cl))))
        `(let ((,size (t.compute-store-size ,cl ,count)))
           (with-foreign-object (,point ,type ,size)
             (let ((,var (make-instance ',(store-type cl) :sap ,point :length ,size)))
               ,@(when init
                       `((lety ((,init_ ,init :type ,(field-type cl)))
                           (loop :for ,idx :from 0 :below (t.store-size ,cl ,var)
                              :do (t.store-set ,cl ,init_ ,var ,idx)))))
               (locally
                   ,@body))))))))

(deftensor cuda-dense-tensor (foreign-dense-tensor cuda-vector-store-mixin)
  ((parent :initform nil :initarg :parent :type (or null tensor) :documentation "This slot is bound if the tensor is the view of another."))
  (:documentation "Object which holds all values of its components, with a simple-vector store."))

(defmethod tensor-generator (field (tensor (eql 'cuda-dense-tensor)))
  (let* ((super-classes (cond 
                          ((member field '(single-float double-float (complex single-float) (complex double-float)) :test #'equal)
                           `(blas-mixin cuda-mixin ,tensor))
                          ;; cuda-vector also supports sizes 8,16
                          ((member field '(character octet unsigned-byte signed-byte fixnum integer))
                           `(cuda-mixin ,tensor))
                          (t `(,tensor))
                          #+nil (case order (1 'vector-mixin) (2 'matrix-mixin))))
         (cl-name (intern (format nil "<~{~a~^ ~}: ~a>" super-classes field) (find-package "OBJ/TENSOR"))))
    (compile-and-eval
     `(progn
        (defclass ,cl-name (,@super-classes) () (:metaclass tensor-class))
        (setf (slot-value (find-class ',cl-name) 'field-type) ',field)))
    cl-name))

(define-template-method t.total-size (sym cuda-dense-tensor) (ele)
  `(vector-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (* x y))) (the index-store-vector (dimensions ,ele))))
