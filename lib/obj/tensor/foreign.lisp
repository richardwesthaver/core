;;; foreign.lisp --- Foreign Tensors

;; 

;;; Code:
(in-package :obj/tensor)

(defclass foreign-vector-store-mixin () ())

;;
(deft/method t.store-type (type foreign-vector-store-mixin) (&optional (size '*))
  (foreign-vector (or (real-subtypep (field-type type)) (field-type type))))
(deft/method t.compute-store-size (cl foreign-vector-store-mixin) (size)
  (if (real-subtypep (field-type cl)) `(* 2 ,size) size))
(deft/method t.store-size (cl foreign-vector-store-mixin) (vec)
  (if (real-subtypep (field-type cl)) `(/ (slot-value (the ,(store-type cl) ,vec) 'length) 2) `(slot-value (the ,(store-type cl) ,vec) 'length)))

(deft/method t.store-ref (class foreign-vector-store-mixin) (store &rest idx)
  (assert (null (cdr idx)) nil "given more than one index for linear-store")
  (let ((idx (car idx)))
    (if (real-subtypep (field-type class))
        (using-gensyms (decl (store idx) (2idx))
          `(let (,@decl)
             (declare (type ,(store-type class) ,store)
                      (type index-type ,idx))
             (let-typed ((,2idx (* 2 ,idx) :type index-type))
               (values (complex (fvref (the ,(store-type class) ,store) ,2idx) (fvref (the ,(store-type class) ,store) (1+ ,2idx))) t))))
        `(values (fvref (the ,(store-type class) ,store) (the index-type ,idx)) t))))

(deft/method t.store-set (class foreign-vector-store-mixin) (value store &rest idx)
  (assert (null (cdr idx)) nil "given more than one index for linear-store")
  (let ((idx (car idx)))
    (if-let ((real-type (real-subtypep (field-type class))))
        (using-gensyms (decl (store idx value) (2idx))
          `(let (,@decl)
             (declare (type ,(store-type class) ,store)
                      (type ,(field-type class) ,value)
                      (type index-type ,idx))
             (let-typed ((,2idx (* 2 ,idx) :type index-type))
               (funcall #'(setf fvref) (the ,real-type (cl:realpart ,value)) (the ,(store-type class) ,store) ,2idx)
               (funcall #'(setf fvref) (the ,real-type (cl:imagpart ,value)) (the ,(store-type class) ,store) (1+ ,2idx)))
             ,value))
        `(funcall #'(setf fvref) (the ,(field-type class) ,value) (the ,(store-type class) ,store) (the index-type ,idx)))))

(deft/method t.store-allocator (type foreign-vector-store-mixin) (size &rest initargs)
  (letv* (((&key (initial-element (coerce 0 (field-type type)))) initargs)
          (element-type (or (real-subtypep (field-type type)) (field-type type))))
    (with-gensyms (sitm len vec idx init sap)
      `(let*-typed ((,len (t.compute-store-size ,type (let ((,sitm ,size))
                                                        (etypecase ,sitm
                                                          (index-type ,sitm)
                                                          (index-store-vector (lvec-foldr #'* (the index-store-vector ,sitm)))
                                                          (cons (reduce #'* ,sitm))))))
                    ,@(when initial-element `((,init ,initial-element :type ,(field-type type))))
                    (,vec (let* ((,sap (cffi:foreign-alloc ,(alien-to-element-type element-type) :count ,len))
                                 (,vec (make-instance (foreign-vector ',element-type) :ptr ,sap :length ,len)))
                            (tg:finalize ,vec #'(lambda () (foreign-free ,sap)))
                            ,vec)))
         ,@(when initial-element
                 `((very-quickly (loop :for ,idx :from 0 :below (t.store-size ,type ,vec)
                                    :do (setf (t.store-ref ,type (the ,(foreign-vector element-type) ,vec) ,idx) (the ,(field-type type) ,init))))))
         ,vec))))
;;
(deft/method with-field-element (cl foreign-vector-store-mixin) (decl &rest body)
  (destructuring-bind (var init &optional (count 1)) decl
    (with-gensyms (idx size point init_)
      (let ((type (element-type (store-type cl))))
        `(let ((,size (t.compute-store-size ,cl ,count)))
           (cffi:with-foreign-object (,point ,type ,size)
             (let ((,var (make-instance ',(store-type cl) :ptr ,point :length ,size)))
               ,@(when init
                       `((let-typed ((,init_ ,init :type ,(field-type cl)))
                           (loop :for ,idx :from 0 :below (t.store-size ,cl ,var)
                              :do (t.store-set ,cl ,init_ ,var ,idx)))))
               (locally
                   ,@body))))))))
;;
(defclass foreign-dense-tensor (dense-tensor foreign-vector-store-mixin)
  ((parent :initform nil :initarg :parent :type (or null tensor) :documentation "This slot is bound if the tensor is the view of another."))
  (:metaclass tensor-class)
  (:documentation "Object which holds all values of its components, with a simple-vector store."))

(defmethod tensor-generator (field (tensor (eql 'foreign-dense-tensor)))
  (let* ((super-classes (remove nil (list (if (member field '(single-float double-float (complex single-float) (complex double-float)) :test #'equal) 'blas-mixin) tensor #+nil (case order (1 'vector-mixin) (2 'matrix-mixin)))))
         (cl-name (intern (format nil "<~{~a~^ ~}: ~a>" super-classes field) (find-package "OBJ/TENSOR"))))
    (compile-and-eval
     `(progn
        (defclass ,cl-name (,@super-classes) () (:metaclass tensor-class))
        (setf (slot-value (find-class ',cl-name) 'field-type) ',field)))
    cl-name))

(deft/method t.total-size (sym foreign-dense-tensor) (ele)
  `(lvec-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (* x y))) (the index-store-vector (dimensions ,ele))))
;;
(definline make-foreign-dense-tensor (dimensions sap &optional (type 'double-float)
                                                 &aux (dimensions (copy-seq (coerce (ensure-list dimensions) 'index-store-vector))))
  (letv* ((str nz (make-stride dimensions)))
    (make-instance (tensor type 'foreign-dense-tensor)
                   :dimensions dimensions :strides str :head 0
                   :store (etypecase sap
                            (foreign-vector sap)
                            (system-area-pointer (make-instance (foreign-vector type) :ptr sap :length nz))))))
;;
