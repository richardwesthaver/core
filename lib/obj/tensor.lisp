;;; tensor.lisp --- Tensor Objects

;; 

;;; Code:
(in-package :obj/tensor)

;;; Vars
(defparameter *default-sparse-store-increment* 100
  "Determines the increment by which the store of a compressed sparse matrix is
increased, when it runs out of store.")

(defparameter *default-sparsity* 1/1000
  "Determines the default sparsity for a newly created sparse matrix, when the
number of non-zero is not specified.")

(defparameter *max-sparse-size* 10000
  "Upper bounds the store size for a newly created sparse matrix, when the number
of non-zero is not specified.")

;;Default ordering of strides
(eval-always
  (defparameter *default-stride-ordering* :col-major
    "Determines whether strides are row or column major by default.

(let ((*default-stride-ordering* :col-major))
   (make-real-tensor 10 10))
;; returns a 10x10 matrix in Column major order."))

(defparameter *default-tensor-type* 'real-tensor)

(defparameter *check-after-initializing-p* t
  "If non-nil, then check for invalid values in the field of the class in the
:after specialized method (if defined), else do nothing. One ought to be very
carful when doing, much of Matlisp's code is written on the assumption that
the fields of a tensor don't take invalid values; failing which case, may lead
to memory error. Use at your own risk.")

;;; Types
(deftype index-type () 'fixnum)

(deftype index-store-vector (&optional (size '*)) `(simple-array index-type (,size)))

(make-array-allocator allocate-index-store 'index-type 0
                      "Allocate index storage")

(definline make-index-store (contents)
  "Allocate index storage with initial elements from the list CONTENTS."
  (the index-store-vector (make-array (length contents) :element-type 'index-type
                                                        :initial-contents contents)))

(definline idxv (&rest contents)
  (make-index-store contents))

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

(definline rank (tensor)
  (declare (type base-tensor tensor))
  (length (the index-store-vector (slot-value tensor 'dimensions))))

(declaim (ftype (function (base-tensor &optional index-type) (or index-type index-store-vector)) dimensions))
(definline dimensions (x &optional idx)
  (declare (type base-tensor x))
  (if idx
      (the index-type (aref (the index-store-vector (slot-value x 'dimensions)) (modproj (or idx 0) (rank x) nil 0)))
      (the index-store-vector (slot-value x 'dimensions))))

(defmethod make-load-form ((tensor base-tensor) &optional env)
  (make-load-form-saving-slots tensor :environment env))

(defgeneric size (obj)
  (:method ((tensor base-tensor))
    (vector-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (* x y))) (the index-store-vector (dimensions tensor))))
  (:method ((obj sequence))
    (length obj))
  (:method ((arr array))
    (reduce #'* (array-dimensions arr))))

(definline dims (tensor &optional idx)
  (declare (type base-tensor tensor))
  (if idx (aref (dimensions tensor) (modproj (or idx 0) (rank tensor) nil 0))
      (vector-to-list (the index-store-vector (dimensions tensor)))))

(defclass sparse-tensor (base-tensor) ())
(defclass dense-tensor (base-tensor) ())

(defgeneric ref (tensor &rest subscripts)
  (:documentation "Return the element from TENSOR corresponding to SUBSCRIPTS"))

(defgeneric (setf ref) (value tensor &rest subscripts))

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
;;
(defgeneric store-ref (tensor idx)
  (:documentation  "Generic serial read access to the store.")
  (:method ((tensor base-tensor) idx)
    (let ((clname (class-name (class-of tensor))))
      ;; (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
      (compile-and-eval
       `(defmethod store-ref ((tensor ,clname) idx)
          (t.store-ref ,clname (store tensor) idx))))
    (store-ref tensor idx)))

(defgeneric (setf store-ref) (value tensor idx)
  (:method (value (tensor base-tensor) idx)
    (let ((clname (class-name (class-of tensor))))
      ;; (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
      (compile-and-eval
       `(defmethod (setf store-ref) (value (tensor ,clname) idx)
          (t.store-set ,clname value (store tensor) idx)
          (t.store-ref ,clname (store tensor) idx))))
    (setf (store-ref tensor idx) value)))

;;
(defgeneric store-size (tensor)
  (:documentation "
  Syntax
  ======
  (store-size tensor)

  Purpose
  =======
  Returns the number of elements the store of the tensor can hold
  (which is not necessarily equal to its vector length).")
  (:method ((tensor base-tensor))
    (let ((clname (class-name (class-of tensor))))
      (compile-and-eval
       `(defmethod store-size ((tensor ,clname))
          (t.store-size ,clname (store tensor))))
      (store-size tensor))))

(defgeneric subtensor (tensor subscripts)
  (:documentation "Creates a new tensor data structure, sharing store with TENSOR but with
different strides and dimensions, as defined in the subscript-list SUBSCRIPTS.

Examples:
(defvar X (make-real-tensor 10 10 10))
;; X

;; Get (: 0 0)
(subtensor X '((nil nil . nil) (0 1 . nil) (0 1 . nil)))
;; Get (: 2:5 :)
(subtensor X '((nil nil . nil) (2 5 . nil)))
;; Get (: : 0:2:10) (0:10:2 = [i : 0 <= i < 10, i % 2 = 0])
(subtensor X '((nil nil . nil) (nil nil . nil) (0 10 . 2)))

Sadly in our parentheses filled world, this function has to be necessarily
verbose (unlike MATLAB, Python). However, this function has been designed with
the express purpose of using it with a Lisp reader macro. The slicing
semantics is essentially the same as MATLAB except for the zero-based
indexing.")
  (:method :before ((tensor base-tensor) (subscripts list))
    (assert (or (null subscripts) (= (length subscripts) (rank tensor))) nil 'tensor-index-rank-mismatch)))

(defun (setf subtensor) (value tensor subscripts)
  (copy value (subtensor tensor subscripts)))

;;; Internal Tensor Protocols
(macrolet ((defn (sym num args &body body)
             `(definline ,(symbolicate 't. (string sym)) ,(cons (car num) args)
                (declare ,(reverse num) (optimize (speed 3) (space 0)))
                ,@body))
           (def-marith (tname clop)
             `(defn ,tname (num number) (&rest nums)
                (if (and (consp num) (eql (first num) 'mod))
                    `(mod (,',clop ,@(mapcar #'(lambda (x) `(the ,num ,x)) nums)) ,(second num))
                    `(,', clop ,@(mapcar #'(lambda (x) `(the ,num ,x)) nums)))))
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
(defgeneric fc (x)
  (:method ((x complex))
    (conjugate x))
  (:method ((x real))
    x)
  (:method ((x t))
    (let ((clname (class-name (class-of x))))
      (compile-and-eval
       `(defmethod fconj ((x ,clname))
          (t.fc ,clname x)))
      (fc x))))
(defun field-realp (fil)
  (eql (macroexpand-1 `(t.fc ,fil phi)) 'phi))
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
      `(mod (coerce ,val 'fixnum ,(second ty)))
      `(coerce ,val ',ty)))

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
  (macroexpand-1 `(t.field-type ,clname)))

(definline t.store-allocator (sym size &optional initial-element)
  (typecase sym
    (standard-tensor
     (with-gensyms (sitm size-sym arr idx init)
       (let ((type (macroexpand-1 `(t.store-element-type ,sym))))
         `(let*-typed ((,size-sym (t.compute-store-size ,sym (let ((,sitm ,size))
                                                               (etypecase ,sitm
                                                                 (index-type ,sitm)
                                                                 (index-store-vector (lvec-foldr #'* (the index-store-vector ,sitm)))
                                                                 (cons (reduce #'* ,sitm))))))
                       ,@(when initial-element `((,init ,initial-element :type ,(field-type sym))))
                       (,arr (make-array ,size-sym :element-type ',type :initial-element ,(if (subtypep type 'number) `(t.fid+ ,type) nil)) :type ,(store-type sym)))
                      ,@(when initial-element
                          `((loop :for ,idx :from 0 :below ,size-sym
                                  :do (t.store-set ,sym ,init ,arr ,idx))))
                      ,arr))))))

(definline t.store-type (sym &optional size))

(defun store-type (cl &optional (size '*))
  (macroexpand-1 `(t.store-stype ,cl ,size)))

(definline t.store-ref (sym store &rest idx)
  (typecase sym
    (linear-store (assert (null (cdr idx)) nil "given more than one index for linear-store")
     `(aref (the ,(store-type sym) ,store) (the index-type ,(car idx))))))

(definline t.store-set (sym value store &rest idx)
  (typecase sym
    (linear-store
     (assert (null (cdr idx)) nil "given more than one index for linear-store")
     `(setf (aref (the ,(store-type sym) ,store) (the index-type ,(car idx))) (the ,(field-type sym) ,value)))))

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
  (macroexpand-1 `(t.field-type ,sym)))

(defun store-element-type (clname)
  (macroexpand-1 `(t.store-element-type ,clname)))

(definline t.compute-store-size (sym &optional (size '*))
  (typecase sym
    (standard-tensor `(simple-array ,(store-element-type sym) (,size)))))

(definline t.store-size (sym ele)
  (typecase sym
    (standard-tensor `(lemgth ,ele))))

(defun with-field-element (sym decl &rest body)
  (destructuring-bind (var init &optional (count 1)) decl
    `(lety ((,var (t.store-allocator ,sym ,count ,init) :type ,(store-type sym)))
           ,@body)))

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

;;Is it a tensor, a linear-store ? It is both!
(defclass standard-tensor (dense-tensor linear-store) ())

(defmethod initialize-instance :after ((tensor standard-tensor) &rest initargs)
  (declare (ignore initargs))
  (when *check-after-initializing-p*
    (lety ((dims (dimensions tensor) :type index-store-vector))
      (assert (>= (head tensor) 0) nil 'tensor-invalid-head-value :head (head tensor) :tensor tensor)
      (if (not (slot-boundp tensor 'strides))
          (multiple-value-bind (stds size) (make-stride dims)
            (declare (type index-store-vector stds)
                     (type index-type size))
            (setf (slot-value tensor 'strides) stds)
            (assert (<= (+ (head tensor) size) (store-size tensor)) nil 'tensor-insufficient-store :store-size (store-size tensor) :max-idx (+ (head tensor) (1- (size tensor))) :tensor tensor))
            (lety ((stds (strides tensor) :type index-store-vector))
              (loop :for i :of-type index-type :from 0 :below (rank tensor)
                 :for sz :of-type index-type := (aref dims 0) :then (the index-type (* sz (aref dims i)))
                 :summing (the index-type (the index-type (* (aref stds i) (1- (aref dims i))))) :into lidx :of-type index-type 
                 :do (assert (> (aref dims i) 0) nil 'tensor-invalid-dimension-value :argument i :dimension (aref dims i) :tensor tensor)
                 :finally (assert (>= (the index-type (store-size tensor)) (the index-type (+ (the index-type (head tensor)) lidx)) 0) nil 'tensor-insufficient-store :store-size (store-size tensor) :max-idx (the index-type (+ (head tensor) lidx)) :tensor tensor)))))))

(defmethod ref ((tensor standard-tensor) &rest subscripts)
  (let ((clname (class-name (class-of tensor))))
    ;; (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
    (compile-and-eval
     `(defmethod ref ((tensor ,clname) &rest subscripts)
        (let ((subs (if (numberp (car subscripts)) subscripts (car subscripts))))
          (t.store-ref ,clname (store tensor) (store-indexing subs tensor)))))
    (apply #'ref (cons tensor subscripts))))

(defmethod (setf ref) (value (tensor standard-tensor) &rest subscripts)
  (let ((clname (class-name (class-of tensor))))
    ;; (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
    (compile-and-eval
     `(defmethod (setf ref) (value (tensor ,clname) &rest subscripts)
        (let* ((subs (if (numberp (car subscripts)) subscripts (car subscripts)))
               (idx (store-indexing subs tensor))
               (sto (store tensor)))
          (t.store-set ,clname (t.coerce ,(field-type clname) value) sto idx)
          (t.store-ref ,clname sto idx))))
    (setf (ref tensor (if (numberp (car subscripts)) subscripts (car subscripts))) value)))

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

;;; LOOPY
(defmacro mod-dotimes ((idx dims &key (loop-order *default-stride-ordering* loop-ordering-p) (uplo? :ul)) &body body)
  "
(mod-dotimes (idx {seq} &key loop-order uplo?) compound-form*)

The argument LOOP-ORDER can either take the keywords {:ROW-MAJOR, :COL-MAJOR},
or the pindex-store-vector corresponding to a permutation action. In the
latter case an array of the form [0..n] is mutated using APPLY-ACTION!, and
parsed left-to-right order.

The argument UPLO? can take one of the keywords {:UL, :U, :L}. If :UL is used,
the loop run around every point in the cube; if :U is specified only the
indices defined by upper simplex, including the diagonal, is generated;
similarly for :L, the lower simplex is generated. Using either {:U, :L} with
UPLO?, automatically sets the loop-ordering, for expected results. If an
argument to LOOP-ORDER is also specified along with UPLO?, then you'll in
general see things which may have weird effects on the control flow.

Make sure that \"do\" is specified at the end: the parser stops at the first
'do it finds.

(mod-dotimes (var {seq})
  {with (linear-sums
            {(offsets {stride-seq})}*)}
  {do ({code}*)})

Examples:
(mod-dotimes (idx (idxv 2 2))
  with (linear-sums (of (idxv 2 1)))
  do (format t \"~a ~a~%\" idx of))
#(0 0) 0
#(0 1) 1
#(1 0) 2
#(1 1) 3

(mod-dotimes (idx (idxv 2 2) :loop-order :col-major)
  with (linear-sums (of (idxv 2 1)))
  do (format t \"~a ~a~%\" idx of))
#(0 0) 0
#(1 0) 2
#(0 1) 1
#(1 1) 3"
  (check-type idx symbol)
  (unless loop-ordering-p
    (ecase uplo? (:ul nil) (:u (setq loop-order :col-major)) (:l (setq loop-order :row-major))))
  (labels ((parse-code (body ret)
             (cond
               ((null body)
                (values nil ret))
               ((member (car body) '(with :with))
                (multiple-value-bind (indic decl) (parse-with (cadr body))
                  (setf (getf ret indic) (append (getf ret indic) decl)))
                (parse-code (cddr body) ret))
               ((member (car body) '(do :do))
                (values (cadr body) ret))
               (t (error 'unknown-token :token (car body) :message "Error in macro: mod-dotimes -> parse-code.~%"))))
           (parse-with (code)
             (cond
               ((member (car code) '(linear-sums :linear-sums))
                (values :linear-sums
                        (loop :for decl :in (cdr code)
                              :collect (destructuring-bind (offst strds &optional (init 0)) decl
                                         (list :offset-sym offst
                                               :offset-init init
                                               :stride-sym (gensym (concatenate 'string (symbol-name offst) "-stride"))
                                               :stride-expr strds)))))
               (t (error 'unknown-token :token (car code) :message "Error in macro: mod-dotimes -> parse-with.~%")))))
    (multiple-value-bind (code sdecl) (parse-code body nil)
      (let ((loop-perm (unless (member loop-order '(:row-major :col-major))
                         ;;Assumed to be a permutation action store
                         (prog1 loop-order
                           (setq loop-order nil)))))
        (with-gensyms (perm-sym loopi-sym dims-sym rank-sym count-sym)
          `(let ((,dims-sym ,dims))
             (declare (type index-store-vector ,dims-sym))
             (let ((,rank-sym (length ,dims-sym))
                   ,@(when loop-perm
                       `((,perm-sym ,loop-perm))))
               (declare (type index-type ,rank-sym)
                        ,@(when loop-perm
                            `((type pindex-store-vector ,perm-sym))))
               ,@(when loop-perm
                   `((assert (<= (length ,perm-sym) ,rank-sym) nil 'permutation-permute-error)))
               (let ((,idx (allocate-index-store ,rank-sym))
                     ,@(when loop-perm `((,loopi-sym (allocate-index-store ,rank-sym))))
                     ,@(mapcar #'(lambda (x) `(,(getf x :stride-sym) ,(getf x :stride-expr))) (getf sdecl :linear-sums))
                     ,@(mapcar #'(lambda (x) `(,(getf x :variable) ,(getf x :init))) (getf sdecl :variables)))
                 (declare (type index-store-vector ,idx ,@(when loop-perm `(,loopi-sym)))
                          ,@(when (getf sdecl :linear-sums)
                              `((type index-store-vector ,@(mapcar #'(lambda (x) (getf x :stride-sym)) (getf sdecl :linear-sums)))))
                          ,@(loop :for x :in (getf sdecl :variables)
                                  :unless (null (getf x :type))
                                  :collect `(type ,(getf x :type) ,(getf x :variable))))
                 ,@(when loop-perm
                     `((loop :for i :of-type index-type :from 0 :below ,rank-sym :do (setf (aref ,loopi-sym i) i))
                       (apply-action! ,loopi-sym ,perm-sym)))
                 (loop ,@(loop :for decl :in (getf sdecl :linear-sums)
                               :append `(:with ,(getf decl :offset-sym) :of-type index-type := ,(getf decl :offset-init)))
                          ,@(unless (null code)
                              `(:do (,@code)))
                       :while ,(append
                                (if loop-perm
                                    `(loop :for ,count-sym :of-type index-type :across ,loopi-sym)
                                    (ecase loop-order
                                      (:row-major `(loop :for ,count-sym :of-type index-type :from (1- ,rank-sym) :downto 0))
                                      (:col-major `(loop :for ,count-sym :of-type index-type :from 0 :below ,rank-sym))))
                                `(:do
                                  (if ,(recursive-append (ecase uplo?
                                                           (:ul nil)
                                                           (:l `(or (and (> ,count-sym 0) (= (aref ,idx ,count-sym) (aref ,idx (1- ,count-sym))))))
                                                           (:u `(or (and (< ,count-sym (1- ,rank-sym)) (= (aref ,idx ,count-sym) (aref ,idx (1+ ,count-sym)))))))
                                                         `(= (aref ,idx ,count-sym) (1- (aref ,dims-sym ,count-sym))))
                                      (progn
                                        ,@(loop
                                            :for decl :in (getf sdecl :linear-sums)
                                            :collect (let ((cstrd (gensym (concatenate 'string "cur-" (symbol-name (getf decl :stride-sym))))))
                                                       `(let ((,cstrd (aref ,(getf decl :stride-sym) ,count-sym)))
                                                          (declare (type index-type ,cstrd))
                                                          (unless (= ,cstrd 0)
                                                            (decf ,(getf decl :offset-sym) (the index-type (* ,cstrd (aref ,idx ,count-sym))))))))
                                        (setf (aref ,idx ,count-sym) 0))
                                      (progn
                                        (incf (aref ,idx ,count-sym))
                                        ,@(loop
                                            :for decl :in (getf sdecl :linear-sums)
                                            :collect (let ((cstrd (gensym (concatenate 'string "cur-" (symbol-name (getf decl :stride-sym))))))
                                                       `(let ((,cstrd (aref ,(getf decl :stride-sym) ,count-sym)))
                                                          (declare (type index-type ,cstrd))
                                                          (unless (= ,cstrd 0)
                                                            (incf ,(getf decl :offset-sym) ,cstrd)))))
                                        (return t)))
                                  :finally (return nil))))))))))))

(defmacro dorefs ((idx dims &key (loop-order *default-stride-ordering* loop-ordering-p) (uplo? :ul)) (&rest ref-decls) &rest body)
  (let* ((tsyms (std/list:zipsym (mapcar #'second ref-decls)))
         (rsyms (mapcar #'car ref-decls))
         (types (mapcar #'(lambda (x) (destructuring-bind (ref ten &key type) x
                                        (declare (ignore ref ten))
                                        type))
                        ref-decls))
         (ssyms (mapcar #'(lambda (x y) (when y `(,(gensym) (store ,(car x))))) tsyms types))
         (osyms (mapcar #'(lambda (y) (when y (gensym))) types)))
    `(let-typed (,@(mapcar #'(lambda (x y) (if y (append x `(:type ,y)) x)) tsyms types))
                (let-typed (,@(remove-if #'null (mapcar #'(lambda (x y) (when y (append x `(:type ,(store-type y))))) ssyms types)))
                           (mod-dotimes (,idx ,dims ,@(when loop-ordering-p `(:loop-order ,loop-order)) :uplo? ,uplo?)
                             :with (linear-sums
                                    ,@(remove-if #'null (mapcar #'(lambda (of ten typ) (when typ `(,of (strides ,(car ten)) (head ,(car ten)))))
                                                                osyms tsyms types)))
                             :do (symbol-macrolet (,@(mapcar #'(lambda (ref sto ten of typ) (if typ
                                                                                                (list ref `(the ,(field-type typ) (t.store-ref ,typ ,(car sto) ,of)))
                                                                                                (list ref `(ref ,(car ten) ,idx))))
                                                             rsyms ssyms tsyms osyms types))
                                   ,@body))))))

(defmacro list-loop ((idx ele lst) &rest body)
  "
  (list-loop (idx ele {list}) compound-form*)

  Examples:
  > (list-loop (idx ele '((1 2) (4 5)))
      with (linear-sums (of (idxv 2 1)))
      do (format t \"~a ~a ~a~%\" idx of ele))
  #(0 0) 0 1
  #(0 1) 1 2
  #(1 0) 2 4
  #(1 1) 3 5
"
  (check-type idx symbol)
  (check-type ele symbol)
  (labels ((parse-code (body ret)
             (cond
               ((null body)
                (values nil ret))
               ((eq (car body) 'with)
                (multiple-value-bind (indic decl) (parse-with (cadr body))
                  (setf (getf ret indic) decl))
                (parse-code (cddr body) ret))
               ;;Let's not do too much.
               #+nil
               ((eq (car body) 'finally)
                (setf (getf ret :finally) (second body))
                (parse-code (cddr body) ret))
               ((eq (car body) 'do)
                (values (cadr body) ret))
               (t (error 'unknown-token :token (car body) :message "Error in macro: mod-dotimes -> parse-code.~%"))))
           (parse-with (code)
             (cond
               ((eq (car code) 'linear-sums)
                (values :linear-sums
                        (loop for decl in (cdr code)
                              collect (destructuring-bind (offst strds &optional (init 0)) decl
                                        (list :offset-sym offst
                                              :offset-init init
                                              :stride-sym (gensym (concatenate 'string (symbol-name offst) "-stride"))
                                              :stride-expr strds)))))
               ;;Traversing the list the other way is far too inefficient and/or too hard to do.
               #+nil
               ((and (eq (car code) 'loop-order)
                     (member (cadr code) '(:row-major :col-major)))
                (values :loop-order (second code)))
               ;;Useless without a finally clause.
               #+nil
               ((eq (car code) 'variables)
                (values :variables
                        (loop for decl in (cdr code)
                              collect (destructuring-bind (sym init &key type) decl
                                        (list :variable sym
                                              :init init
                                              :type type)))))
               (t (error 'std/condition:unknown-token :token (car code) :message "Error in macro: mod-dotimes -> parse-with.~%")))))
    (multiple-value-bind (code sdecl) (parse-code body nil)
      (with-gensyms (lst-sym dims-sym rank-sym lst-rec-sym lst-rec-count-sym lst-rec-lst-sym)
        `(let ((,lst-sym ,lst))
           (declare (type list ,lst-sym))
           (let ((,dims-sym (make-index-store (list-dimensions ,lst-sym))))
             (declare (type index-store-vector ,dims-sym))
             (let ((,rank-sym (array-dimension ,dims-sym 0)))
               (declare (type index-type ,rank-sym))
               (let ((,idx (allocate-index-store ,rank-sym))
                     ,@(mapcar #'(lambda (x) `(,(getf x :offset-sym) ,(getf x :offset-init))) (getf sdecl :linear-sums))
                     ,@(mapcar #'(lambda (x) `(,(getf x :stride-sym) ,(getf x :stride-expr))) (getf sdecl :linear-sums))
                     ,@(mapcar #'(lambda (x) `(,(getf x :variable) ,(getf x :init))) (getf sdecl :variables)))
                 (declare (type index-store-vector ,idx)
                          ,@(when (getf sdecl :linear-sums)
                              `((type index-store-vector ,@(mapcar #'(lambda (x) (getf x :stride-sym)) (getf sdecl :linear-sums)))
                                (type index-type ,@(mapcar #'(lambda (x) (getf x :offset-sym)) (getf sdecl :linear-sums)))))
                          ,@(loop for x in (getf sdecl :variables)
                                  unless (null (getf x :type))
                                  collect `(type ,(getf x :type) ,(getf x :variable))))
                 (labels ((,lst-rec-sym (,lst-rec-count-sym ,lst-rec-lst-sym)
                            (if (null ,lst-rec-lst-sym)
                                (progn
                                  (unless (= (aref ,idx ,lst-rec-count-sym) (aref ,dims-sym ,lst-rec-count-sym))
                                    (error 'non-uniform-bounds-error :assumed (aref ,dims-sym ,lst-rec-count-sym) :found ,lst-rec-count-sym
                                                                     :message "Error in list-loop, given list is not uniform in dimensions."))
                                  (setf (aref ,idx ,lst-rec-count-sym) 0)
                                  ,@(loop
                                      for decl in (getf sdecl :linear-sums)
                                      collect `(decf ,(getf decl :offset-sym) (the index-type (* (aref ,(getf decl :stride-sym) ,lst-rec-count-sym) (aref ,dims-sym ,lst-rec-count-sym)))))
                                  ,@(if (null (getf sdecl :finally))`(nil)
                                        `((when (= ,lst-rec-count-sym 0)
                                            ,(getf sdecl :finally)))))
                                (progn
                                  ;;list-dimensions does not parse the entire list, just goes through caaa..r's to find out the
                                  ;;dimensions if it is uniform.
                                  (unless (< -1 (aref ,idx ,lst-rec-count-sym) (aref ,dims-sym ,lst-rec-count-sym))
                                    (error 'out-of-bounds-error :requested (aref ,idx ,lst-rec-count-sym) :bound (aref ,dims-sym ,lst-rec-count-sym)
                                                                :message "Error in list-loop, given list is not uniform in dimensions."))
                                  (if (consp (car ,lst-rec-lst-sym))
                                      (,lst-rec-sym (1+ ,lst-rec-count-sym) (car ,lst-rec-lst-sym))
                                      (let ((,ele (car ,lst-rec-lst-sym)))
                                        ,code))
                                  (incf (aref ,idx ,lst-rec-count-sym))
                                  ,@(loop
                                      for decl in (getf sdecl :linear-sums)
                                      collect `(incf ,(getf decl :offset-sym) (the index-type (aref ,(getf decl :stride-sym) ,lst-rec-count-sym))))
                                  (,lst-rec-sym ,lst-rec-count-sym (cdr ,lst-rec-lst-sym))))))
                   (,lst-rec-sym 0 ,lst-sym))))))))))

;;; Einstein
;;; COPY
(defmethod copy ((from array) (to array))
  (let ((lst (make-list (array-rank to))))
    (mod-dotimes (idx (make-index-store (array-dimensions to)))
      do (progn
           (copy-vector-to-list idx lst)
           (setf (apply #'aref to lst) (apply #'aref from lst)))))
  to)

(defmethod copy ((from t) (to array))
  (let ((lst (make-list (array-rank to))))
    (mod-dotimes (idx (make-index-store (array-dimensions to)))
      do (progn
           (copy-vector-to-list idx lst)
           (setf (apply #'aref to lst) from)))
    to))

;;
(defmethod copy :before ((x array) (y standard-tensor))
  (assert (tree-equal (array-dimensions x) (vector-to-list (dimensions y)))
          nil 'dimension-mismatch))
(defmethod copy :before ((x standard-tensor) (y array))
  (assert (tree-equal (array-dimensions y) (vector-to-list (dimensions x)))
          nil 'dimension-mismatch))

(defmethod copy ((x array) (y standard-tensor))
  (let ((clname (class-name (class-of y))))
    ;; (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
    (compile-and-eval
     `(defmethod copy ((x array) (y ,clname))
        (lety ((sto-y (store y) :type (simple-array ,(store-element-type clname)))
                    (lst (make-list (array-rank x)) :type cons))
                   (mod-dotimes (idx (dimensions y))
                     with (linear-sums
                           (of-y (strides y) (head y)))
                     do (t.store-set ,clname (t.coerce ,(field-type clname) (apply #'aref x (lvec->list! idx lst))) sto-y of-y)))
        y))
    (copy x y)))

(defmethod copy ((x standard-tensor) (y array))
  (let ((clname (class-name (class-of x))))
    (compile-and-eval
     `(defmethod copy ((x ,clname) (y array))
        (let-typed ((sto-x (store x) :type (simple-array ,(store-element-type clname)))
                    (lst (make-list (array-rank y)) :type cons))
                   (mod-dotimes (idx (dimensions x))
                     with (linear-sums
                            (of-x (strides x) (head x)))
                     do (setf (apply #'aref y (lvec->list! idx lst)) (t.store-ref ,clname sto-x of-x))))
        y))
    (copy x y)))

(defmethod copy ((x cons) (y standard-tensor))
  ;;You seriously weren't expecting efficiency were you :) ?
  (let ((arr (make-array (list-dimensions x) :initial-contents x)))
    (copy arr y)))

;;; SWAP

;;; PRINT
;;; Coordinate Sparse
;;; Compressed Sparse

;;; Utils
(defmacro with-rowm (&rest body)
  `(let ((*default-stride-ordering* :row-major))
     ,@body))

(defmacro with-colm (&rest body)
  `(let ((*default-stride-ordering* :col-major))
     ,@body))
