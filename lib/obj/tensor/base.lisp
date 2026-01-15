;;; base.lisp --- Tensor Base

;; 

;;; Code:
(in-package :obj/tensor)

;;; Types
(deftype index-type () 'fixnum)
(deftype index-store (&optional (size '*)) `(simple-array index-type ,size))
(deftype index-store-vector (&optional (size '*)) `(simple-array index-type (,size)))
(deftype index-store-matrix (&optional (m '*) (n '*)) `(simple-array index-type (,m ,n)))

;;; Base Accessor
(defclass base-accessor ()
  ((dimensions :initarg :dimensions :type index-store-vector :documentation "Dimensions of the vector spaces in which the tensor's arguments reside.")))

(declaim (ftype (function (base-accessor &optional (or boolean index-type)) (or index-store-vector index-type list)) dimensions))
(definline dimensions (x &optional idx)
  (declare (type base-accessor x))
  (typecase idx
    (null (the index-store-vector (slot-value x 'dimensions)))
    (index-type (the index-type (aref (the index-store-vector (slot-value x 'dimensions)) (modproj (or idx 0) (order x) nil 0))))
    (t (loop for e across (the index-store-vector (slot-value x 'dimensions)) collect e))))

;;Can this be moved out into MOP ?
(defmethod initialize-instance :after ((x base-accessor) &rest initargs)
  (declare (ignore initargs))
  (when *tensor-safety-p*
    (locally (declare (optimize (speed 3) (safety 0)))
      (loop :for i :from 0 :below (length (dimensions x))
         :do (assert (> (dimensions x i) 0) nil 'tensor-invalid-dimension-value :argument i :dimension (dimensions x i) :tensor x)))))

;;We use order (opposed to CL convention) so as not to cause confusion with matrix rank.
(definline order (x)
  (declare (type base-accessor x))
  (length (the index-store-vector (slot-value x 'dimensions))))

(defclass stride-accessor (base-accessor)
  ((strides :initarg :strides :type index-store-vector :documentation "Strides for accesing elements of the tensor.")
   (head :initarg :head :initform 0 :type index-type :documentation "Head for the store's accessor."))
  (:documentation "Vanilla stride accessor."))
(defclass coordinate-accessor (base-accessor)
  ((indices :initarg :indices :type index-store-matrix :documentation "Non-zero indices in the tensor.")
   (strides :initarg :strides :type index-store-vector :documentation "Strides for accesing elements of the tensor.")
   (stride-hash :initarg :stride-hash :type index-store-vector :documentation "Strides in Column major order")
   (tail :initform 0 :initarg :boundary :type index-type :documentation "Row bound for indices"))
  (:documentation "Bi-partite graph/Hypergraph/Factor/Co-ordinate store"))
(defclass graph-accessor (base-accessor)
  ((fence :initarg :fence :type index-store-vector :documentation "Start index for neighborhood.")
   (neighbors :initarg :neighbors :type index-store-vector :documentation "Neighbor ids.")
   (transposep :initarg :transposep :initform nil :type boolean :documentation "Choose between row-column compressed forms."))
  (:documentation "Graph store via Adjacency lists; only works for matrices."))

;;Store types
(defclass simple-vector-store-mixin () ())
(defclass hash-table-store-mixin () ())

;;; Base Tensor
(defclass base-tensor () ()
  (:documentation "Base tensor instance class."))

(defmethod make-load-form ((tensor base-tensor) &optional env)
  (make-load-form-saving-slots tensor :environment env))

(deftensor tensor (base-tensor base-accessor)
  ((store :initarg :store :reader store :documentation "Storage for the tensor.")
   (memos :initform nil :documentation "Memoized attributes."))
  (:documentation "Object which directly holds the values of its components (or part thereof)."))

(defmethod make-load-form ((tensor base-tensor) &optional env)
  (make-load-form-saving-slots tensor :environment env))
(declaim (ftype (function (tensor) hash-table) memos))
(definline memos (x)
  (declare (type tensor x))
  (or (slot-value x 'memos) (setf (slot-value x 'memos) (make-hash-table :test 'equal))))

(deftype sparse-tensor () `(or coordinate-tensor hash-tensor graph-tensor))

(deftensor stride-tensor (tensor stride-accessor) ())

(deftensor dense-tensor (stride-tensor)
  ((parent :initform nil :initarg :parent :type (or null tensor) :documentation "This slot is bound if the tensor is the view of another."))
  (:documentation "Object which holds all values of its components."))
(deftensor hash-tensor (stride-tensor hash-table-store-mixin)
  ((stride-pivot :initarg :stride-pivot :type index-store-vector :documentation "This slot is used to invert the hash.")))

(definline orphanize (x)
  (declare (type dense-tensor))
  (setf (slot-value x 'parent) nil) x)

(deftensor simple-dense-tensor (dense-tensor simple-vector-store-mixin) ()
  (:documentation "Dense tensor with simple-vector store."))
(defclass blas-mixin () ()
  (:documentation "Mixin which indicates that there exist foreign-routines for an object of this type."))

(deftensor graph-tensor (tensor graph-accessor simple-vector-store-mixin) ())

(deftensor coordinate-tensor (tensor coordinate-accessor simple-vector-store-mixin) ())

(defclass vector-mixin () ())
(defclass matrix-mixin () ())

(defun tensor-typep (tensor subs)
  "Check if the given tensor is of a particular size in particular arguments.

Checking for a vector:
(tensor-typep ten '(class-name *))

Checking for a matrix with 2 columns:
(tensor-typep ten '(real-tensor (* 2)))"
  (declare (type base-tensor tensor))
  (destructuring-bind (cls &optional subscripts) (ensure-list subs)
    (and (typep tensor cls)
         (if subscripts
             (lety ((rank (order tensor) :type index-type)
                    (dims (dimensions tensor) :type index-store-vector))
                   (loop for val in subscripts
                         for i of-type index-type = 0 then (1+ i)
                         do (unless (or (eq val '*) (eq val (aref dims i)))
                               (return nil))
                         finally (return (when (= (1+ i) rank) t))))
             t))))

(definline tensor-matrixp (ten)
  (declare (type base-accessor ten))
  (= (order ten) 2))

(definline tensor-vectorp (ten)
  (declare (type base-accessor ten))
  (= (order ten) 1))

(definline tensor-squarep (tensor)
  (declare (type base-accessor tensor))
  (lety ((dims (dimensions tensor) :type index-store-vector))
    (loop for i from 1 below (length dims)
          do (unless (= (aref dims i) (aref dims 0))
               (return nil))
          finally (return t))))

(deftype tensor-vector () `(and tensor (satisfies tensor-vectorp)))
(deftype tensor-matrix () `(and tensor (satisfies tensor-matrixp)))
(deftype tensor-square-matrix () `(and tensor (satisfies tensor-matrixp) (satisfies tensor-squarep)))

(deftype tensor-type (field &key tensor order square)
  (let ((types (remove nil
                       (list (tensor field (case tensor (* nil)))
                             (ecase order
                               (1 `(satisfies tensor-vectorp))
                               (2 `(satisfies tensor-matrixp))
                               (* nil))
                             (ecase square
                               ((or '* nil) nil)
                               (t `(satisfies tensor-squarep)))))))
    (if (cdr types) (list* 'and types) (car types))))

(defgeneric tensor-generator (field tensor))

(with-memoization ()
  (memoizing
   (defun tensor (field &optional tensor &aux (tensor (or tensor 'simple-dense-tensor)))
     (or (when-let ((class (find field (class-direct-subclasses (find-class tensor))  :key #'(lambda (x) (if (slot-boundp x 'field-type) (field-type (class-name x)))))))
           (class-name class))
         (tensor-generator field tensor)))))

(defmethod tensor-generator (field (tensor (eql 'simple-dense-tensor)))
  (let* ((super-classes 
           (remove nil (list (if (member field 
                                         '(single-float double-float (complex single-float) (complex double-float))
                                         :test #'equal)
                                 'blas-mixin)
                             tensor
                             #+nil (case order (1 'vector-mixin) (2 'matrix-mixin)))))
         (cl-name (intern (format nil "<~{~a~^ ~}: ~a>" super-classes field) (find-package "OBJ/TENSOR"))))
    (compile-and-eval
     `(progn
        (defclass ,cl-name (,@super-classes) () (:metaclass tensor-class))
        (setf (slot-value (find-class ',cl-name) 'field-type) ',field)))
    cl-name))

(defmethod tensor-generator (field (tensor symbol))
  (assert (member tensor #1='(graph-tensor hash-tensor coordinate-tensor)) (tensor) 'std:invalid-argument
          :reason (format nil "TENSOR must be one of: ~A" #1#)
          :item tensor)
  (let* ((super-classes (list tensor #+nil (case order (1 'vector-mixin) (2 'matrix-mixin))))
         (cl-name (intern (format nil "<~{~a~^ ~}: ~a>" super-classes field) (find-package "OBJ/TENSOR"))))
    (compile-and-eval
     `(progn
        (defclass ,cl-name (,@super-classes) ()
          (:metaclass tensor-class))
        (setf (slot-value (find-class ',cl-name) 'field-type) ',field)))
    cl-name))

;;This is useful for Eigenvalue decompositions
(defgeneric complexified-tensor (class)
  (:method ((class-name symbol))
    (complexified-tensor (find-class class-name))))

(with-memoization ()
  (memoizing
   (defmethod complexified-tensor ((class tensor-class))
     (cond
       ((subtypep (field-type class) 'cl:complex) class)
       ((subtypep (field-type class) 'cl:real)
        (let* ((field `(cl:complex ,(field-type class)))
               (super-classes (class-direct-superclasses class))
               (siblings (apply #'intersection (mapcar #'class-direct-subclasses super-classes))))
          (or (find field siblings :test #'equal :key #'field-type)
              (let ((cl-name (intern (format nil "<~{~a~^ ~}: ~a>" (mapcar #'class-name super-classes) field) (find-package "OBJ/TENSOR"))))
                (compile-and-eval
                 `(prog1
                      (defclass ,cl-name (,@(mapcar #'class-name super-classes)) ()
                        (:metaclass tensor-class))
                    (setf (slot-value (find-class ',cl-name) 'field-type) ',field)))))))
       (t (error "Unknown complex tensor for ~a" class))))))
(defmethod complexified-tensor :around ((class tensor-class))
  (class-name (call-next-method)))

(defgeneric realified-tensor (class)
  (:method ((class-name symbol))
    (realified-tensor (find-class class-name)))
  (:method :around ((class tensor-class))
    (class-name (call-next-method))))
(with-memoization ()
  (memoizing
   (defmethod realified-tensor ((class tensor-class))
     (cond
       ((subtypep (field-type class) 'cl:real) class)
       ((and (listp (field-type class)) (= (length (field-type class)) 2))
        (check-type (car (field-type class)) cl:complex)
        (letv* ((super-classes (class-direct-superclasses class))
                (siblings (reduce #'intersection (mapcar #'class-direct-subclasses super-classes)))
                (field (second (field-type class))))
          (or (find field siblings :test #'equal :key #'field-type)
              (let ((cl-name (intern (format nil "<~{~a~^ ~}: ~a>" (mapcar #'class-name super-classes) field) (find-package "OBJ/TENSOR"))))
                (compile-and-eval
                 `(prog1
                      (defclass ,cl-name (,@(mapcar #'class-name super-classes)) ()
                        (:metaclass tensor-class))
                    (setf (slot-value (find-class ',cl-name) 'field-type) ',field)))))))
       (t (error "Unknown real tensor for ~a" class))))))
