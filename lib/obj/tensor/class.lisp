;;; class.lisp --- Tensor Classes

;; 

;;; Code:
(in-package :obj/tensor)

;;; Tensor Classes
;;;; Numeric
(defclass numeric-tensor (standard-tensor) ())
;; (deft t.field-type (sym numeric-tensor) ()
;;   'number)
(defclass real-numeric-tensor (numeric-tensor) ())
;; (deft t.field-type (sym real-numeric-tensor) ()
;;   'real)
;; (deft t.realified-type (sym real-numeric-tensor) ()
;;   sym)

(defclass rational-tensor (real-numeric-tensor) ())
;; (deft t.field-type (sym rational-tensor) ()
;;   'rational)

(defclass fixnum-tensor (real-numeric-tensor) ())
;; (deft t.field-type (sym fixnum-tensor) () 'fixnum)

(defclass octet-tensor (real-numeric-tensor) ())
;; (deft t.field-type (sym octet-tensor) () '(unsigned-byte 8))

(defclass boolean-tensor (real-numeric-tensor) ())
;; (deft t.field-type (sym boolean-tensor) () '(mod 2))

(defclass blas-numeric-tensor (numeric-tensor) ())

(defclass real-blas-tensor (real-numeric-tensor blas-numeric-tensor) ())

(defmethod print-element ((tensor real-blas-tensor)
                          element stream)
  (format stream "~,4,-2,,,,'Eg" element))

(defclass real-tensor (real-blas-tensor) ())
;; (deft t.field-type (sym real-tensor) () 'double-float)
;; (deft t.complexified-type (sym real-tensor) () 'complex-tensor)

(defclass sreal-tensor (real-blas-tensor) ())
;; (deft t.field-type (sym sreal-tensor) () 'single-float)
;; (deft t.complexified-type (sym sreal-tensor) () 'scomplex-tensor)

(defclass complex-numeric-tensor (numeric-tensor) ())
;; (deft t.field-type (sym complex-numeric-tensor) () 'complex)
;; (deft t.complexified-type (sym complex-numeric-tensor) () sym)

(defclass complex-blas-tensor (complex-numeric-tensor blas-numeric-tensor) ())

(defmethod store-size ((tensor complex-blas-tensor))
  (floor (/ (length (store tensor)) 2)))

(defmethod print-element ((tensor complex-blas-tensor)
                          element stream)
  (let ((realpart (realpart element))
        (imagpart (imagpart element)))
    (if (not (zerop imagpart))
        (format stream "~,4,-2,,,,'Eg ~a ~,4,-2,,,,'Egi"  realpart (if (>= imagpart 0) #\+ #\-) (abs imagpart))
        (format stream "~,4,-2,,,,'Eg" realpart))))

(defclass complex-tensor (complex-blas-tensor) ())
;; (deft t.field-type (sym complex-tensor) () '(complex double-float))
;; (deft t.realified-type (sym complex-tensor) () 'real-tensor)

(defclass scomplex-tensor (complex-blas-tensor) ())
;; (deft t.field-type (sym scomplex-tensor) () '(complex single-float))
;; (deft t.realified-type (sym scomplex-tensor) () 'sreal-tensor)

;;;; Sparse
(defclass real-coordinate-sparse-tensor (coordinate-sparse-tensor) ())
;; (deft t.field-type (sym real-coordinate-sparse-tensor) () 'double-float)

(defclass real-compressed-sparse-matrix (compressed-sparse-matrix) ())
;; (deft t.field-type (sym real-compressed-sparse-matrix) () 'double-float)

;;; Coordinate Sparse
(defclass coordinate-sparse-tensor (sparse-tensor)
  ((head :initarg :head :initform 0 :reader head :type index-type
         :documentation "Head for the store's accessor.")
   (strides :initarg :strides :type index-store-vector
            :documentation "Strides for accesing elements of the tensor.")))

;; (deft t.sparse-fill sparse-tensor (sym)
;;  `(t.fid+ (t.field-type ,sym)))

;; (deft t.store-allocator coordinate-sparse-tensor (sym size &optional nz)
;;   (with-gensyms (size-sym)
;;     `(let ((,size-sym (or ,nz (min (max 16 (ceiling (/ ,size *default-sparsity*))) *max-sparse-size*))))
;;        (make-hash-table :size ,size-sym))))

;; (deft t.store-ref coordinate-sparse-tensor (sym store &rest idx)
;;    (assert (null (cdr idx)) nil "given more than one index for hashtable.")
;;   `(the ,(field-type sym) (gethash ,(car idx) ,store (t/sparse-fill ,sym))))

;; (deft t.store-set coordinate-sparse-tensor (sym value store &rest idx)
;;    (assert (null (cdr idx)) nil "given more than one index for hashtable.")
;;    (with-gensyms (val)
;;      `(let-typed ((,val ,value :type ,(field-type sym)))
;;         (unless (t/f= ,(field-type sym) ,val (t/fid+ ,(field-type sym)))
;;           (setf (gethash ,(car idx) ,store) (the ,(field-type sym) ,value))))))

;; (deft t.store-type coordinate-sparse-tensor (sym &optional (size '*))
;;   'hash-table)

;; (deft t.store-size coordinate-sparse-tensor (sym ele)
;;   `(hash-table-count ,ele))

;; (deft t.store-type coordinate-sparse-tensor (sym &optional (size '*))
;;   'hash-table)
;;
(defmethod ref ((tensor coordinate-sparse-tensor) &rest subscripts)
  (let ((clname (class-name (class-of tensor))))
    ;; (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
    (compile-and-eval
     `(defmethod ref ((tensor ,clname) &rest subscripts)
        (let ((subs (if (numberp (car subscripts)) subscripts (car subscripts))))
          (t.store-ref ,clname (store tensor) (store-indexing subs tensor)))))
    (apply #'ref (cons tensor subscripts))))

(defmethod (setf ref) (value (tensor coordinate-sparse-tensor) &rest subscripts)
  (let ((clname (class-name (class-of tensor))))
    ;; (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
    (compile-and-eval
     `(defmethod (setf ref) (value (tensor ,clname) &rest subscripts)
        (let* ((subs (if (numberp (car subscripts)) subscripts (car subscripts)))
               (idx (store-indexing subs tensor))
               (sto (store tensor)))
          (t.store-set ,clname (t/coerce ,(field-type clname) value) sto idx)
          (t.store-ref ,clname sto idx))))
    (setf (ref tensor (if (numberp (car subscripts)) subscripts (car subscripts))) value)))

;;; Compressed Sparse
(defclass compressed-sparse-matrix (sparse-tensor)
  ((transposed :initform nil :initarg :transposed :reader transposed :type boolean
               :documentation "If NIL the matrix is in CSC, else if T, then matrix is CSR.")
   (neighbour-start :initarg :neighbour-start :reader neighbour-start :type index-store-vector
                    :documentation "Start index for ids and store.")
   (neighbour-id :initarg :neighbour-id :reader neighbour-id :type index-store-vector
                 :documentation "Row id.")))

(declaim (ftype (function (compressed-sparse-matrix) index-store-vector) neighbour-start neighbour-id))

(defun compressed-sparse-indexing (subs tensor)
  (declare (type compressed-sparse-matrix tensor)
           (type (or index-store-vector cons) subs))
  (lety ((row 0 :type index-type)
         (col 0 :type index-type))
        (etypecase subs
          (cons
           (assert (null (cddr subs)) nil 'tensor-index-rank-mismatch)
           (setf row (the index-type (car subs))
                 col (the index-type (cadr subs))))
          (index-store-vector
           (assert (= (length subs) 2) nil 'tensor-index-rank-mismatch)
           (setf row (the index-type (aref subs 0))
                 col (the index-type (aref subs 1)))))
        (when (transposed tensor)
          (rotatef row col))
        (lety* ((nst (neighbour-start tensor) :type index-store-vector)
                (nid (neighbour-id tensor) :type index-store-vector)
                (lb (aref nst col) :type index-type)
                (ub (aref nst (1+ col)) :type index-type))
               (declare (type index-type row col))
               (if (or (= lb ub) (< row (aref nid lb)) (> row (aref nid (1- ub)))) (values -1 row col)
                   (values
                    (loop :with j := (ash (+ lb ub) -1)
                          :repeat 64
                          :do (cond
                                ((= (aref nid j) row) (return j))
                                ((>= lb (1- ub)) (return -1))
                                (t
                                 (if (< row (aref nid j))
                                     (setf ub j)
                                     (setf lb (1+ j)))
                                 (setf j (ash (+ lb ub) -1)))))
                    row col)))))

;; FIX 2025-05-22: 
;; (deft t.store-allocator (cl compressed-sparse-matrix) (size &optional nz)
;;   (let ((sto-type (store-element-type cl)))
;;     `(destructuring-bind (nr nc) ,size
;;        (let ((nz (or ,nz (min (ceiling (* nr nc *default-sparsity*)) *max-sparse-size*))))
;;          (list
;;           (allocate-index-store nz)
;;           (make-array (t/compute-store-size ,cl nz) :element-type ',sto-type :initial-element ,(if (subtypep sto-type 'number) `(t/fid+ ,sto-type) nil)))))))

;; (deft t.compute-store-size (sym compressed-sparse-matrix) (size)
;;   size)
;; ;;
;; (deft t.store-type (sym compressed-sparse-matrix) (&optional (size '*))
;;   `(simple-array ,(store-element-type sym) (,size)))

;; (deft t.store-ref (sym compressed-sparse-matrix) (store &rest idx)
;;    (assert (null (cdr idx)) nil "given more than one index for compressed-store")
;;   `(aref (the ,(store-type sym) ,store) (the index-type ,(car idx))))

;; (deft t.store-set (sym compressed-sparse-matrix) (value store &rest idx)
;;    (assert (null (cdr idx)) nil "given more than one index for compressed store")
;;   `(setf (aref (the ,(store-type sym) ,store) (the index-type ,(car idx))) (the ,(field-type sym) ,value)))

;; (deft t.store-size (sym compressed-sparse-matrix) (ele)
;;   `(length ,ele))

;; (deft t.store-element-type (sym compressed-sparse-matrix) ()
;;   (macroexpand `(t/field-type ,sym)))
;;
(defmethod ref ((tensor compressed-sparse-matrix) &rest subscripts)
  (let ((clname (class-name (class-of tensor))))
    ;; (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
    (compile-and-eval
     `(defmethod ref ((tensor ,clname) &rest subscripts)
        (let ((idx (compressed-sparse-indexing (if (numberp (car subscripts)) subscripts (car subscripts)) tensor)))
          (if (< idx 0)
              (values (t.sparse-fill ,clname) nil)
              (values (t.store-ref ,clname (store tensor) idx) t)))))
    (apply #'ref (cons tensor subscripts))))

(defmethod (setf ref) (value (tensor compressed-sparse-matrix) &rest subscripts)
  (let ((clname (class-name (class-of tensor))))
    ;; (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
    (compile-and-eval
     `(defmethod (setf ref) (value (tensor ,clname) &rest subscripts)
        (multiple-value-bind (idx row col) (compressed-sparse-indexing (if (numberp (car subscripts)) subscripts (car subscripts)) tensor)
          (declare (type index-type idx row col))
          (lety ((value (t/coerce ,(field-type clname) value) :type ,(field-type clname)))
                (if (/= value (t/fid+ ,(field-type clname)))
                    (if (< idx 0)
                        (let* ((ns (neighbour-start tensor))
                               (value (t/coerce ,(field-type clname) value))
                               (row-data (let ((ni (neighbour-id tensor))
                                               (vi (store tensor)))
                                           (merge 'list
                                                  (list (cons row value))
                                                  (loop :for j :from (aref ns col) :below (aref ns (1+ col))
                                                        :collect (cons (aref ni j) (aref vi j)))
                                                  #'< :key #'car))))
                          (unless (> (store-size tensor) (aref ns (1- (length ns))))
                            (destructuring-bind (ni vi) (t.store-allocator ,clname (dims tensor) (+ (store-size tensor) *default-sparse-store-increment*))
                              (let ((nio (neighbour-id tensor))
                                    (vio (store tensor)))
                                (very-quickly
                                  (declare (type index-store-vector nio ni ns)
                                           (type ,(store-type clname) vio vi))
                                  (loop :for i :from 0 :below (aref ns col)
                                        :do (setf (aref nio i) (aref ni i)
                                                  (aref vio i) (aref vi i)))
                                  (loop :for i :from (aref ns (1+ col)) :below (aref ns (1- (length ns)))
                                        :do (setf (aref nio (1+ i)) (aref ni i)
                                                  (aref vio (1+ i)) (aref vi i))))
                                (setf (slot-value tensor 'neighbour-id) ni
                                      (slot-value tensor 'store) vi))))
                          (let ((ni (neighbour-id tensor))
                                (vi (store tensor)))
                            (very-quickly
                              (declare (type index-store-vector ni ns)
                                       (type ,(store-type clname) vi))
                              (loop :for i :from (1+ col) :below (length ns)
                                    :do (incf (aref ns i))))
                            (loop :for (r . v) :in row-data
                                  :for i := (aref ns col) :then (1+ i)
                                  :do (setf (aref ni i) r
                                            (aref vi i) v))))
                        (t.store-set ,clname value (store tensor) idx))
                    (when (>= idx 0)
                      (let ((ns (neighbour-start tensor))
                            (ni (neighbour-id tensor))
                            (vi (store tensor)))
                        (very-quickly
                          (declare (type index-store-vector ns ni)
                                   (type ,(store-type clname) vi))
                          (loop :for i :from idx :below (aref ns (1- (length ns)))
                                :do (setf (aref ni i) (aref ni (1+ i))
                                          (aref vi i) (aref vi (1+ i))))
                          (loop :for i :from (1+ col) :below (length ns)
                                :do (decf (aref ns i)))))))
                value))))
    (setf (ref tensor (if (numberp (car subscripts)) subscripts (car subscripts))) value)))
