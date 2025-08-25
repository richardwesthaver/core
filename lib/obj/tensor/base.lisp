;;; base.lisp --- Tensor Base

;; 

;;; Code:
(in-package :obj/tensor)

;;; Utils
(make-array-allocator allocate-index-store 'index-type 0
                      "Allocate index storage")

(definline make-index-store (contents)
  "Allocate index storage with initial elements from the list CONTENTS."
  (the index-store-vector (make-array (length contents) :element-type 'index-type
                                                        :initial-contents contents)))

(declaim (inline simple-array-type))
(defun simple-array-type (sym &optional (size '*))
  "Create the list representing simple-array with type SYM."
  `(simple-array ,sym (,size)))

;;; Base Tensor
(defclass base-tensor ()
  ((dimensions :initarg :dimensions :type index-store-vector
               :documentation "Dimensions of the vector spaces in which the tensor's arguments reside.")
   ;; (parent-tensor :reader parent-tensor :initform nil :initarg :parent-tensor :type (or null base-tensor)
   ;;                :documentation "If the tensor is a view of another tensor, then this slot is bound.")
   (store :initarg :store :reader store
          :documentation "The actual storage for the tensor.")
   ;; (attributes :initarg :attributes :initform nil
   ;;             :documentation "Place for computable attributes of an object instance.")
   )
  (:documentation "Basic tensor class."))

(defclass sparse-tensor (base-tensor) ())
(defclass dense-tensor (base-tensor) ())

(defmethod make-load-form ((tensor base-tensor) &optional env)
  (make-load-form-saving-slots tensor :environment env))

(defmethod print-element ((tensor base-tensor) element stream)
  (format stream "~a" element))

(definline rank (tensor)
  (declare (type base-tensor tensor))
  (length (the index-store-vector (slot-value tensor 'dimensions))))

(declaim (ftype (function (base-tensor &optional index-type) (or index-type index-store-vector)) dimensions))
(definline dimensions (x &optional idx)
  (declare (type base-tensor x))
  (if idx
      (the index-type (aref (the index-store-vector (slot-value x 'dimensions)) (modproj (or idx 0) (rank x) nil 0)))
      (the index-store-vector (slot-value x 'dimensions))))

(defmethod size ((tensor base-tensor))
  (vector-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (* x y))) (the index-store-vector (dimensions tensor))))

(definline dims (tensor &optional idx)
  (declare (type base-tensor tensor))
  (if idx (aref (dimensions tensor) (modproj (or idx 0) (rank tensor) nil 0))
      (vector-to-list (the index-store-vector (dimensions tensor)))))

(labels ((array-subs (obj subscripts)
           (let ((subs (etypecase (car subscripts)
                         (number subscripts)
                         (cons (car subscripts))
                         (vector (vector-to-list (car subscripts))))))
             (loop for s on subs
                   for i = 0 then (1+ i)
                   do (when (< (car s) 0)
                        (rplaca s (modproj (car s) (array-dimension obj i) nil))))
             subs)))
  (defmethod ref ((obj array) &rest subscripts)
    (apply #'aref obj (array-subs obj subscripts)))
  (defmethod (setf ref) (value (obj array) &rest subscripts)
    (apply #'(setf aref) value obj (array-subs obj subscripts))))

(labels ((list-subs (obj subscripts)
           (let ((subs (etypecase (car subscripts)
                         (number subscripts)
                         (cons (car subscripts))
                         (vector (vector-to-list (car subscripts))))))
             (assert (= (length subs) 1) nil 'invalid-argument) (setf subs (car subs))
             (when (< subs 0) (setf subs (modproj subs (length obj))))
             subs)))
  (defmethod ref ((obj cons) &rest subscripts)
    (elt obj (list-subs obj subscripts)))
  (defmethod (setf ref) (value (obj cons) &rest subscripts)
    (setf (elt obj (list-subs obj subscripts)) value)))

(defmethod store-ref ((tensor base-tensor) idx)
  (let ((clname (class-name (class-of tensor))))
    (t.store-ref clname (store tensor) idx)))

(defmethod (setf store-ref) (value (tensor base-tensor) idx)
  (let ((clname (class-name (class-of tensor))))
    (t.store-set clname value (store tensor) idx)
    (t.store-ref clname (store tensor) idx)))

(defmethod store-size ((tensor base-tensor))
    (let ((clname (class-name (class-of tensor))))
      (t.store-size clname (store tensor))))

(defmethod subtensor :before ((tensor base-tensor) (subscripts list))
  (assert (or (null subscripts) (= (length subscripts) (rank tensor))) nil 'tensor-index-rank-mismatch))

(defun (setf subtensor) (value tensor subscripts)
  (copy value (subtensor tensor subscripts)))

(definline parse-slice (subs dimensions)
  (declare (type index-store-vector dimensions))
  (let ((dims) (psubs))
    (loop for sub.i in subs
          for d of-type index-type across dimensions
          do (if (not (consp sub.i))
                 (let ((idx (modproj (the (or index-type null) sub.i) d nil 0)))
                   (push 1 dims)
                   (push idx psubs))
                 (destructuring-bind (start end . inc) sub.i
                   (declare ((or index-type null) start end inc))
                   (let* ((inc (modproj inc nil nil 1))
                          (start (modproj start d nil (if (> inc 0) 0 (1- d))))
                          (end (modproj end d t (if (> inc 0) d -1)))
                          (nd (ceiling (- end start) inc)))
                     (declare (type index-type start end inc nd))
                     (when (<= nd 0) (return nil))
                     (push nd dims)
                     (push (list* start end inc) psubs))))
          finally (return (values (nreverse psubs) (nreverse dims))))))

(definline parse-slice-for-strides (subscripts dimensions strides)
  (declare (type index-store-vector dimensions strides)
           (type list subscripts))
  (let ((dims) (stds))
    (loop for sub.i in subscripts
          for d across dimensions
          for s across strides
          with hd = 0
          if (not (consp sub.i))
          do (let ((idx (modproj (the (or index-type null) sub.i) d nil 0)))
               (incf hd (* s idx)))
          else do
             (destructuring-bind (start end . inc) sub.i
               (declare ((or index-type null) start end inc))
               (let* ((inc (modproj inc nil nil 1))
                      (start (modproj start d nil (if (> inc 0) 0 (1- d))))
                      (end (modproj end d t (if (> inc 0) d -1)))
                      (nd (ceiling (- end start) inc)))
                 (declare (type index-type start end inc nd))
                 (when (<= nd 0) (return nil))
                 (incf hd (* s start))
                 (push nd dims)
                 (push (* inc s) stds)))
          finally (return (values hd (nreverse dims) (nreverse stds))))))

(definline slice (x axis &optional (idx 0) (preserve-rank-p (when (= (rank x) 1) t)))
  (let* ((axis (modproj axis (rank x) nil 0))
         (subs (loop for i from 0 below (rank x) 
                     collect (cond ((/= i axis) '(nil nil))
                                   (preserve-rank-p (list idx (1+ idx)))
                                   (t idx)))))
    (subtensor x subs)))

(definline row-slice (x idx)
  (slice x 0 idx))

(definline col-slice (x idx)
  (slice x 1 idx))

(defmethod suptensor :before ((tensor base-tensor) ord &optional (start 0))
  (declare (type index-type start))
  (let ((tord (rank tensor)))
    (assert (and (< -1 start) (<= tord (rank tensor)) (<= 0 start (- ord tord))) nil 'invalid-arguments)))

(definline matrixify (vec &optional (col-vectorp t))
  (if (tensor-matrixp vec) vec (suptensor vec 2 (if col-vectorp 0 1))))

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
             (lety ((rank (rank tensor) :type index-type)
                    (dims (dimensions tensor) :type index-store-vector))
                   (loop :for val :in subscripts
                         :for i :of-type index-type := 0 :then (1+ i)
                         :do (unless (or (eq val '*) (eq val (aref dims i)))
                               (return nil))
                         :finally (return (when (= (1+ i) rank) t))))
             t))))

(definline tensor-matrixp (ten)
  (declare (type base-tensor ten))
  (= (rank ten) 2))

(definline tensor-vectorp (ten)
  (declare (type base-tensor ten))
  (= (rank ten) 1))

(deftype base-square-matrix ()
  `(and base-tensor (satisfies tensor-square-matrixp)))

(deftype base-matrix ()
  `(and base-tensor (satisfies tensor-matrixp)))

(deftype base-vector ()
  `(and base-tensor (satisfies tensor-vectorp)))

(definline tensor-squarep (tensor)
  (declare (type base-tensor tensor))
  (lety ((dims (dimensions tensor) :type index-store-vector))
        (loop :for i :from 1 :below (length dims)
              :do (unless (= (aref dims i) (aref dims 0))
                    (return nil))
              :finally (return t))))

(defun tensor-append (axis tensor &rest more-tensors)
  (if (null tensor)
      (when more-tensors
        (apply #'tensor-append axis (car more-tensors) (cdr more-tensors)))
      (let ((dims (copy-seq (dimensions tensor))))
        (loop for ele in more-tensors do (incf (aref dims axis) (aref (dimensions ele) axis)))
        (let* ((ret (%zeros dims (class-of tensor)))
               (view (slice ret axis 0 t)))
          (loop for ele in (cons tensor more-tensors)
                with head = 0
                do
                   (progn
                     (setf (slot-value view 'head) head
                           (aref (dimensions view) axis) (aref (dimensions ele) axis))
                     (copy ele view)
                     (incf head (* (aref (strides ret) axis) (aref (dimensions ele) axis)))))
          ret))))

;;; Internal Tensor Protocols
(macrolet ((defn (sym num args &body body)
             `(definline ,(symbolicate 't. (string sym)) ,(cons (car num) args)
                (declare ,(reverse num) (optimize (speed 3) (space 0)))
                ,@body))
           (def-marith (tname clop)
             `(defn ,tname (num number) (&rest nums)
                `(,',clop ,@(mapcar #'(lambda (x) `(the ,num ,x)) nums))))
           (genarith (&rest args)
             `(progn ,@(mapcar #'(lambda (x) `(def-marith ,(car x) ,(cadr x))) args))))
  (genarith (f+ +) (f- -) (f* *) (f= =) (f/ /)))

(definline t.fid+ (ty)
  (coerce 0 ty))
(definline t.fid* (ty)
  (coerce 1 ty))
(definline t.fc (ty)
  (etypecase ty
    (real ty)
    (t (conjugate ty))))
(defmethod fc ((x t))
  (let ((clname (class-name (class-of x))))
    (t.fc clname)))
(defun field-realp (fil)
  (eql (t.fc fil) 'phi))
(definline t.frealpart (ty)
  (etypecase ty
    (real ty)
    (t (realpart ty))))
(definline t.fimagpart (ty)
  (etypecase ty
    (real (t.fid+ 'real))
    (t (imagpart ty))))
(definline t.coerce (val ty)
  (if (and (consp ty) (eql (first ty) 'mod))
      (mod (coerce val 'fixnum) (second ty))
      (coerce val ty)))

;; HACK 2025-05-22: strict-coerce
;; (defun strict-compare (func-list a b)
;;   (loop :for func :in func-list
;;         :for elea :in a
;;         :for eleb :in b
;;         :do (unless (funcall func elea eleb)
;;               (return nil))
;;         :finally (return t)))

;; (defun dict-compare (func-list a b)
;;   (loop :for func :in func-list
;;         :for elea :in a
;;         :for eleb :in b
;;         :do (when (funcall func elea eleb)
;;               (return t))))

;;;; Tensor Specialization
(definline t.field-type (sym)
  (typecase sym
    (base-tensor t)))
(defun field-type (clname)
  (t.field-type clname))

(definline t.store-allocator (sym size &optional (initial-element 0))
  (typecase sym
    (standard-tensor
     (let ((type (t.store-element-type sym)))
       (lety* ((size-sym (t.compute-store-size sym (let ((sitm size))
                                                          (etypecase sitm
                                                            (index-type sitm)
                                                            (index-store-vector (vector-foldr 
                                                                                 #'* 
                                                                                 (the index-store-vector sitm)))
                                                            (cons (reduce #'* sitm))))))
               (init initial-element)
               (arr (make-array size-sym :element-type type :initial-element (if (subtypep type 'number) (t.fid+ type) nil))))
              (when initial-element
                (loop :for idx :from 0 :below size-sym
                      :do (t.store-set sym init arr idx)))
              arr)))))

(definline t.store-type (sym &optional (size '*))
  (typecase sym
    (standard-tensor
     (simple-array-type (store-element-type sym) size))))

(defun store-type (cl &optional (size '*))
  (t.store-type cl size))

(definline t.store-ref (sym store &rest idx)
  (typecase sym
    (linear-store (assert (null (cdr idx)) nil "given more than one index for linear-store")
     (aref store (the index-type (car idx))))))

(definline t.store-set (sym value store &rest idx)
  (typecase sym
    (linear-store
     (assert (null (cdr idx)) nil "given more than one index for linear-store")
     (setf (aref store (the index-type (car idx))) value))))

(define-setf-expander t.store-ref (sym store &rest idx &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion store env)
    (declare (ignore newval setter))
    (with-gensyms (nval)
      (values dummies
              vals
              `(,nval)
              `(t.store-set ,sym ,nval ,getter ,@idx)
              `(t.store-ref ,sym ,getter ,@idx)))))

(definline t.store-element-type (sym)
  (t.field-type sym))

(defun store-element-type (clname)
  (t.store-element-type clname))

(definline t.compute-store-size (sym size)
  (typecase sym
    (standard-tensor size)))

(definline t.store-size (sym ele)
  (typecase sym
    (standard-tensor (length ele))))

(defun with-field-element (sym decl &rest body)
  (destructuring-bind (var init &optional (count 1)) decl
    `(lety ((,var (t.store-allocator ,sym ,count ,init) :type ,(store-type sym)))
           ,@body)))

(defmacro with-field-elements (sym decls &rest body)
  (if (null decls) `(progn ,@body)
      `(with-field-element ,sym ,(first decls)
         (with-field-elements ,sym ,(cdr decls) ,@body))))

(defparameter *tensor-methods* (make-hash-table))

(definline lazy-coerce (x out)
  (if (typep x out) x
      (copy x out)))

(defun cclass-max (lst)
  (let ((max nil))
    (loop :for ele :in lst
          ;; FIX 2025-05-22: 
          :do (when (or (null max) #+nil (and (coerceable-p max ele)
                                              (or (not (coerceable-p ele max))
                                                  (and (subtypep ele 'blas-numeric-tensor) (subtypep max 'blas-numeric-tensor)
                                                       (> (float-digits (coerce 0 (store-element-type ele)))
                                                          (float-digits (coerce 0 (store-element-type max))))))))
                (setf max ele)))
    max))

(defmacro define-tensor-method (name (&rest args) &body body)
  (let* ((inputs (mapcar #'car (remove-if-not #'(lambda (x) (and (consp x) (eql (third x) :input))) args)))
         (outputs (mapcar #'car (remove-if-not #'(lambda (x) (and (consp x) (eql (third x) :output))) args)))
         (iclsym (zipsym inputs))
         (oclsym (zipsym outputs))
         (dargs (let ((pos (position-if #'(lambda (x) (member x cl:lambda-list-keywords)) args)))
                  (if pos (subseq args 0 pos) args))))
    (with-gensyms (x classes iclasses oclasses)
      `(progn
         (multiple-value-bind (val exists?) (gethash ',name *tensor-methods*)
           (if exists?
               (let ((type-meths (assoc ',(mapcar #'(lambda (x) (if (consp x) (cadr x) t)) dargs) (cdr val) :test #'tree-equal)))
                 (if type-meths
                     (progn
                       (loop :for ele in (cdr type-meths)
                             :do (remove-method (symbol-function ',name) ele))
                       (setf (cdr type-meths) nil))
                     (setf (cdr val) (list* (list ',(mapcar #'(lambda (x) (if (consp x) (cadr x) t)) dargs)) (cdr val)))))
               (setf (gethash ',name *tensor-methods*) (list ',name (list ',(mapcar #'(lambda (x) (if (consp x) (cadr x) t)) dargs))))))
         ;;
         (defmethod ,name (,@(mapcar #'(lambda (x) (if (consp x) (subseq x 0 2) x)) args))
           (let* (,@(mapcar #'(lambda (lst) `(,(car lst) (class-name (class-of ,(cadr lst))))) (append iclsym oclsym))
                  (,iclasses (list ,@(mapcar #'car iclsym)))
                  (,oclasses (list ,@(mapcar #'car oclsym)))
                  (,classes (append ,iclasses ,oclasses)))
             (labels ((generate-code (class)
                        (let ((args (mapcar #'(lambda (x) (if (and (consp x) (member (third x) '(:input :output)))
                                                              (list (car x) class)
                                                              x))
                                            '(,@args)))
                              (ebody (macrolet ((cl (,x)
                                                  (let ((slook '(,@(mapcar #'(lambda (x) `(,(cadr x) class)) iclsym)
                                                                 ,@(mapcar #'(lambda (x) `(,(cadr x) class)) oclsym))))
                                                    (or (cadr (assoc ,x slook)) (error "Can't find class of ~a" ,x)))))
                                       (list ,@body))))
                          `(defmethod ,',name (,@args)
                             ,@ebody))))
               (cond
                 ((every #'(lambda (,x) (eql ,x (car ,classes))) ,classes)
                  ;; (assert (member (car ,classes) *tensor-type-leaves*)
                  ;; nil 'tensor-abstract-class :tensor-class ,classes)
                  (let* ((method (compile-and-eval (generate-code (car ,classes))))
                         (lst (assoc ',(mapcar #'(lambda (x) (if (consp x) (cadr x) t)) dargs) (cdr (gethash ',name *tensor-methods*)) :test #'tree-equal)))
                    (assert lst nil "Method table missing from *tensor-methods*")
                    (setf (cdr lst) (list* method (cdr lst))))
                  (,name ,@(mapcar  #'(lambda (x) (if (consp x) (car x) x)) (remove-if #'(lambda (x) (member x cl:lambda-list-keywords)) args))))
                 ((and (every #'(lambda (,x) (eql ,x (car ,oclasses))) ,oclasses)
                       (or (null ,oclasses) (coerceable-p (cclass-max ,iclasses) (car ,oclasses))))
                  (let* ((clm (or (car ,oclasses) (cclass-max ,iclasses)))
                         ,@(mapcar #'(lambda (x) `(,x (lazy-coerce ,x clm))) inputs))
                    (declare (ignorable clm))
                    (,name ,@(mapcar  #'(lambda (x) (if (consp x) (car x) x)) (remove-if #'(lambda (x) (member x cl:lambda-list-keywords)) args)))))
                 (t
                  (error "Don't know how to apply ~a to classes ~a, ~a." ',name ,iclasses ,oclasses))))))))))

;;;; Standard Tensor
(defclass linear-store ()
  ((head :initarg :head :initform 0 :reader head :type index-type
         :documentation "Head for the store's accessor.")
   (strides :initarg :strides :type index-store-vector
            :documentation "Strides for accesing elements of the tensor.")
   (store :initarg :store :reader store :type vector
          :documentation "The actual storage for the tensor.")))

(declaim (ftype (function (base-tensor &optional index-type) (or index-type index-store-vector)) strides)
	 (ftype (function (base-tensor) index-type) head))

(definline strides (x &optional idx)
  (declare (type base-tensor x))
  (if idx
      (aref (the index-store-vector (slot-value x 'strides)) (modproj (or idx 0) (rank x) nil 0))
      (the index-store-vector (slot-value x 'strides))))

(defun store-indexing-vec (idx hd strides dims)
  "Does error checking to make sure IDX is not out of bounds.

Returns the sum:

  length(STRIDES)
     __
HD + \  STRIDE  * IDX
     /_        i      i
   i = 0"
  (declare (type index-type hd)
	   (type index-store-vector idx strides dims))
  (lety ((rank (length strides) :type index-type))
        (assert (= rank (length idx) (length dims)) nil 'tensor-index-rank-mismatch :index-rank (length idx) :rank rank)
        (loop
	  :for i :of-type index-type :from 0 :below rank
	  :for cidx :across idx
	  :for d :across dims
	  :for s :across strides
	  :with sto-idx :of-type index-type := hd
	  :do (progn
	        (assert (< (1- (- d)) cidx d) nil 'tensor-index-out-of-bounds :argument i :index cidx :dimension d)
	        (incf sto-idx (the index-type (* s (if (< cidx 0) (mod cidx d) cidx)))))
	  :finally (return sto-idx))))

(defun store-indexing-lst (idx hd strides dims)
  "Does error checking to make sure idx is not out of bounds.

Returns the sum:

  length(STRIDES)
     __
HD + \  STRIDE  * IDX
     /_        i      i
   i = 0"
  (declare (type index-type hd)
	   (type index-store-vector strides dims)
	   (type cons idx))
  (lety ((rank (length strides) :type index-type))
        (loop :for cidx :of-type index-type :in idx
	      :for i :of-type index-type := 0 :then (1+ i)
	      :for d :across dims
	      :for s :across strides
	      :with sto-idx :of-type index-type := hd
	      :do (progn
	            (assert (< (1- (- d)) cidx d) nil 'tensor-index-out-of-bounds :argument i :index cidx :dimension d)
	            (incf sto-idx (the index-type (* s (if (< cidx 0) (mod cidx d) cidx)))))
	      :finally (progn
		         (assert (= (1+ i) rank) nil 'tensor-index-rank-mismatch :index-rank (1+ i) :rank rank)
		         (return sto-idx)))))

(definline store-indexing (idx tensor)
  "Returns the linear index of the element pointed by IDX. Does error checking to
make sure idx is not out of bounds.

Returns the sum:

  length(STRIDES)
     __
HD + \  STRIDES  * IDX
     /_        i      i
   i = 0"
  (etypecase idx
    (cons (store-indexing-lst idx (head tensor) (strides tensor) (dimensions tensor)))
    (vector (store-indexing-vec idx (head tensor) (strides tensor) (dimensions tensor)))))

;;Stride makers.
(macrolet ((defstride (fname col?)
	     `(definline ,fname (dims)
		(declare (type index-store-vector dims))
		(lety ((stds (allocate-index-store (length dims)) :type index-store-vector))
		      (loop
		           ,@(if col?
			         `(for i from 0 below (length dims))
			         `(for i from (1- (length dims)) downto 0))
		        with st = 1
		        do (locally (declare (fixnum i st))
                             (lety ((d (aref dims i) :type index-type))
			           (assert (> d 0) nil 'tensor-invalid-dimension-value :argument i :dimension d)
			           (setf (aref stds i) st
			                 st (* st d))))
		        finally (return (values stds st)))))))
  (defstride make-stride-cmj t)
  (defstride make-stride-rmj nil)
  (definline make-stride (dims)
    (ecase *default-stride-ordering* (:row-major (make-stride-rmj dims)) (:col-major (make-stride-cmj dims)))))

;;; Standard Tensor
;;Is it a tensor, a linear-store ? It is both!
(defclass standard-tensor (dense-tensor linear-store) ())

(defmethod initialize-instance :after ((tensor standard-tensor) &rest initargs)
  (declare (ignore initargs))
  (when *tensor-safety-p*
    (lety ((dims (dimensions tensor) :type index-store-vector))
          (assert (>= (head tensor) 0) nil 'tensor-invalid-head-value :head (head tensor) :tensor tensor)
          (if (not (slot-boundp tensor 'strides))
              (multiple-value-bind (stds size) (make-stride dims)
                (declare (index-store-vector stds)
                         (index-type size))
                (setf (slot-value tensor 'strides) stds)
                (assert (<= (+ (head tensor) size) (store-size tensor)) nil 'tensor-insufficient-store :store-size (store-size tensor) :max-idx (+ (head tensor) (1- (size tensor))) :tensor tensor))
              (lety ((stds (strides tensor) :type index-store-vector))
                    (loop :for i :of-type index-type :from 0 :below (rank tensor)
                          :for sz :of-type index-type := (aref dims 0) 
                          :then (the index-type (* sz (aref dims i)))
                          :summing (the index-type (* (aref stds i) (1- (aref dims i)))) :into lidx :of-type index-type
                          :do (assert (> (aref dims i) 0) nil 'tensor-invalid-dimension-value :argument i :dimension (aref dims i) :tensor tensor)
                          :finally (assert (>= (the index-type (store-size tensor)) (the index-type (+ (the index-type (head tensor)) lidx)) 0) nil 'tensor-insufficient-store :store-size (store-size tensor) :max-idx (the index-type (+ (head tensor) lidx)) :tensor tensor)))))))

(defmethod ref ((tensor standard-tensor) &rest subscripts)
  (let ((clname (class-name (class-of tensor))))
    ;; (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
    (let ((subs (if (numberp (car subscripts)) subscripts (car subscripts))))
      (t.store-ref clname (store tensor) (store-indexing subs tensor)))))

(defmethod (setf ref) (value (tensor standard-tensor) &rest subscripts)
  (let ((clname (class-name (class-of tensor))))
    ;; (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
    (let* ((subs (if (numberp (car subscripts)) subscripts (car subscripts)))
           (idx (store-indexing subs tensor))
           (sto (store tensor)))
      (t.store-set clname (t.coerce value (field-type clname)) sto idx)
      (t.store-ref clname sto idx))))

;; (defmethod subtensor ((tensor standard-tensor) (subscripts list))
;;   (multiple-value-bind (hd dims stds) (parse-slice-for-strides subscripts (dimensions tensor) (strides tensor))
;;     (cond
;;       ((not hd) nil)
;;       ((not dims) (if subscripts
;;                       (store-ref tensor hd)
;;                       (with-no-init-checks
;;                           (make-instance (class-of tensor)
;;                                          :head (head tensor)
;;                                          :dimensions (copy-seq (dimensions tensor))
;;                                          :strides (copy-seq (strides tensor))
;;                                          :store (store tensor)
;;                                          :parent-tensor tensor))))
;;       (t (with-no-init-checks
;;              (make-instance (class-of tensor)
;;                             :head (+ hd (head tensor))
;;                             :dimensions (make-index-store dims)
;;                             :strides (make-index-store stds)
;;                             :store (store tensor)
;;                             :parent-tensor tensor))))))

;; (defmethod suptensor ((ten standard-tensor) ord &optional (start 0))
;;   (declare (type index-type ord start))
;;   (if (= (rank ten) ord) ten
;;       (let* ((tord (rank ten)))
;;         (with-no-init-checks
;;             (make-instance (class-of ten)
;;                            :dimensions (make-index-store
;;                                         (nconc (make-list start :initial-element 1)
;;                                                (vector-to-list (dimensions ten))
;;                                                (make-list (- ord tord start) :initial-element 1)))
;;                            :strides (make-index-store
;;                                      (nconc (make-list start :initial-element (size ten))
;;                                             (vector-to-list (strides ten))
;;                                             (make-list (- ord tord start) :initial-element (size ten))))
;;                            :head (head ten)
;;                            :store (store ten)
;;                            :parent-tensor ten)))))

;; (defmethod reshape :before ((tensor standard-tensor) (dims cons))
;;   (assert (loop for s across (strides tensor)
;;                    unless (> (* s (strides tensor 0)) 0) return nil
;;                    finally (return t))
;;           nil 'tensor-error :message "strides are not of the same sign." :tensor tensor)
;;   ;; FIX 2025-05-22: 
;;   (assert (<= (loop for i in dims collect (multiplying i)) (store-size tensor)) nil 'tensor-insufficient-store))

(defmethod reshape ((ten standard-tensor) (dims cons))
  (let ((idim (make-index-store dims)))
    (setf (slot-value ten 'dimensions) idim
          (slot-value ten 'strides) (let ((strd (make-stride idim)))
                                      (when (< (strides ten 0) 0)
                                        (loop for i from 0 below (length strd)
                                              do (setf (aref strd i) (- (aref strd i)))))
                                      strd))
    ten))

;;; Einstein
;;; Permutation
