;;; perm.lisp --- Tensor Permutations

;; 

;;; Code:
(in-package :obj/tensor)

(definline idxv (&rest contents)
  (make-array (length contents) :element-type 'index-type :initial-contents contents))

(definline idxn (n)
  "Create an array with length N with values 0,..,N-1."
  (lety ((ret (t.store-allocator index-store-vector n) :type index-store-vector))
             (locally (declare (optimize (speed 3) (safety 0)))
              (loop :for i :of-type index-type :from 0 :below n
                    :do (setf (aref ret i) i)))
             ret))

(defun pick-random (k n)
  (let ((ret nil)
        (perm (t.store-allocator index-store-vector k)))
    (loop :for i :from 0 :below k
          :do (let ((sd (random (- n i))))
                (loop :for ele :in ret
                      :do (if (> ele sd) (return) (incf sd)))
                (setf (aref perm i) sd)
                (setf ret (merge 'list (list sd) ret #'<))))
    (values ret perm)))

#+nil (sort seq #'> :key #'(lambda (x) (declare (ignore x)) (random 1.0)))

(defun shuffle! (seq)
  "Randomize the elements of a sequence. Destructive on SEQ."
  (let* ((len (length seq))
         (perm (nth-value 1 (pick-random len len))))
    (permute! seq (without-tensor-safety (make-instance 'permutation-action :store perm :size len)))))
;;Class definitions----------------------------------------------;;
(defclass permutation ()
  ((store :reader store :initarg :store)
   (permutation-size :reader permutation-size :initarg :size :type index-type)))

(defmethod print-object ((per permutation) stream)
  (print-unreadable-object (per stream :type t)
    (format stream "S_~a~%" (permutation-size per))
    (if (<= (permutation-size per) 1)
        (format stream "ID~%")
        (format stream "~a~%" (store per)))))

(defclass permutation-index-stored (permutation) ())
;;
(defclass permutation-action (permutation-index-stored)
  ((store :type index-store-vector)))

(defmethod initialize-instance :after ((perm permutation-action) &rest initargs)
  (declare (ignore initargs))
  (when *tensor-safety-p*
    (lety ((repr (store perm) :type index-store-vector))
               (locally (declare (optimize (speed 3) (safety 0)))
                (loop :for i :of-type index-type :from 0 :below (length repr)
                      :with srepr :of-type index-store-vector := (sort (copy-seq repr) #'<)
                      :do (assert (= (aref srepr i) i) nil 'permutation-invalid-error)))
               (setf (slot-value perm 'permutation-size) (length repr)))))

;;
(defclass permutation-cycle (permutation)
  ((store :type list)))

(defmethod initialize-instance :after ((per permutation-cycle) &rest initargs)
  (declare (ignore initargs))
  (when *tensor-safety-p*
    (if (null (store per))
        (setf (slot-value per 'permutation-size) 0)
        (loop
          :for cyc :of-type index-store-vector :in (store per)
          :with ss :of-type index-type := 0
          :do (locally (declare (optimize (speed 3) (safety 0)))
               (loop
                 :for i :of-type index-type :from 1 :below (length cyc)
                 :with scyc :of-type index-store-vector := (sort (copy-seq cyc) #'<)
                 :do (assert (/= (aref scyc (1- i)) (aref scyc i)) nil 'permutation-invalid-error)
                 :finally (setf ss (max ss (aref scyc (1- (length scyc)))))))
          :finally (setf (slot-value per 'permutation-size) (1+ ss))))))

;;
(defclass permutation-pivot-flip (permutation-index-stored)
  ((store :type index-store-vector)))

(defmethod initialize-instance :after ((per permutation-pivot-flip) &rest initargs)
  (declare (ignore initargs))
  (when *tensor-safety-p*
    (lety* ((repr (store per) :type index-store-vector)
                 (len (length repr) :type index-type))
                (locally (declare (optimize (speed 3) (safety 0)))
                 (loop :for i :of-type index-type :from 0 :below len
                       :do (assert (< -1 (aref repr i) len) nil 'permutation-invalid-error)))
                (setf (slot-value per 'permutation-size) len))))
;;

;;Generic permute! method.
(defgeneric permute! (thing permutation &optional argument)
  (:documentation "
  (permute! thing permutation [argument 0])

  Permutes the ARGUMENT index of the the array-like object THING, by
  applying PERMUTATION on it.")
  (:method :before ((seq sequence) (perm permutation) &optional (arg 0))
    (declare (ignore arg))
    (let ((len (length seq)))
      (assert (>= len (permutation-size perm)) nil
              'permutation-permute-error :seq-len len :per-size (permutation-size perm))))
  (:method :before ((ten tensor) (perm permutation) &optional (arg 0))
    (assert (>= (dimensions ten arg) (permutation-size perm)) nil
            'permutation-permute-error :seq-len (dimensions ten arg) :permutation-size (permutation-size perm))))

(definline permute (thing perm &optional (arg 0))
  (permute! (tensor-copy thing) perm arg))

;;Action
(definline apply-action! (seq perm)
  (declare (type vector seq)
           (type index-store-vector perm))
  (let* ((size (length perm))
         (cseq (subseq seq 0 size)))
    (loop :for i :from 0 :below size
          :do (setf (aref seq i) (aref cseq (aref perm i)))
          :finally (return seq))))

(defmethod permute! ((seq cons) (perm permutation-action) &optional arg)
  (declare (ignore arg))
  (let* ((size (permutation-size perm))
         (cseq (coerce (subseq seq 0 size) 'vector))
         (act (store perm)))
    (loop :for i :from 0 :below size
          :for lst := seq :then (cdr lst)
          :do (setf (car lst) (aref cseq (aref act i)))
          :finally (return seq))))

(defmethod permute! ((seq vector) (perm permutation-action) &optional arg)
  (declare (ignore arg))
  (apply-action! seq (the index-store-vector (store perm))))

(defmethod permute! ((ten tensor) (perm permutation-action) &optional (arg 0))
  (permute! ten (tensor-copy perm 'permutation-pivot-flip) arg))
;;Cycle
(definline apply-cycle! (seq pcyc)
  (declare (type index-store-vector pcyc)
           (type vector seq))
  (loop :for i :of-type index-type :downfrom (1- (length pcyc)) :to 1
        :with xl := (aref seq (aref pcyc (1- (length pcyc))))
        :do (setf (aref seq (aref pcyc i)) (aref seq (aref pcyc (1- i))))
        :finally (progn
                   (setf (aref seq (aref pcyc 0)) xl)
                   (return seq))))

(defmethod permute! ((seq cons) (perm permutation-cycle) &optional arg)
  (declare (ignore arg))
  (unless (= (permutation-size perm) 1)
    (let* ((size (permutation-size perm))
           (cseq (coerce (subseq seq 0 size) 'vector)))
      (loop :for cyc :of-type index-store-vector :in (store perm)
            :do (apply-cycle! cseq cyc))
      (loop for ci across cseq 
            for i below size
            for seq* on seq
            do (setf (car seq*) ci))))
  seq)

(defmethod permute! ((seq vector) (perm permutation-cycle) &optional arg)
  (declare (ignore arg))
  (unless (= (permutation-size perm) 1)
    (loop :for cyc :of-type index-store-vector :in (store perm)
          :do (apply-cycle! seq cyc)))
  seq)

(defmethod permute! ((A tensor) (perm permutation-cycle) &optional (arg 0))
  (permute! A (tensor-copy perm 'permutation-pivot-flip) arg))

                                        ;Pivot idx
(definline apply-flips! (seq pflip)
  (declare (type index-store-vector pflip)
           (type vector seq))
  (loop :for i :of-type index-type :from 0 :below (length pflip)
        :unless (= i (aref pflip i))
        :do (rotatef (aref seq i) (aref seq (aref pflip i)))
        :finally (return seq)))

(defmethod permute! ((seq vector) (perm permutation-pivot-flip) &optional arg)
  (declare (ignore arg))
  (apply-flips! seq (store perm)))

(defmethod permute! ((seq cons) (perm permutation-pivot-flip) &optional arg)
  (declare (ignore arg))
  (let* ((size (permutation-size perm))
         (cseq (coerce (subseq seq 0 size) 'vector)))
    (apply-flips! cseq (store perm))
    (loop for ci across cseq
          for i below size
          for seq* on seq
          do (setf (car seq*) ci)))
  seq)

(defmethod permute! ((A tensor) (perm permutation-pivot-flip) &optional (arg 0))
  (lety ((t1 (slice~ A arg)) (t2 (slice~ A arg))
              (idiv (store perm) :type index-store-vector))
             (loop for σi across idiv
                   for i below (length idiv)
                   unless (= i σi)
                   do (progn
                        (setf (slot-value t1 'head) (the index-type (+ (head A) (the index-type (* i (strides A arg)))))
                              (slot-value t2 'head) (the index-type (+ (head A) (the index-type (* σi (strides A arg))))))
                        (swap! t1 t2))))
  A)

;;Conversions----------------------------------------------------;;
(defmethod copy! ((from permutation) (to (eql nil)))
  (copy! from (class-name (class-of from))))

(defmethod copy! ((from permutation) (to permutation))
  (if (typep to (type-of from))
      (copy! (store from) (store to))
      (copy! (store (tensor-copy from (type-of to))) (store to))))

(defmethod copy! ((act permutation-action) (type (eql 'permutation-cycle)))
  (lety ((arr (store act) :type index-store-vector)
              (midx 0 :type index-type))
             (labels ((find-cycle (x0)
                        ;; This function obtains the cycle starting from x_0.
                        (declare (type index-type x0))
                        (if (= (aref arr x0) x0) (values 0 nil)
                            (locally (declare (optimize (speed 3) (safety 0)))
                             (loop
                               :for x :of-type index-type := (aref arr x0) :then (aref arr x)
                               :and ret :of-type cons := (list x0) :then (cons x ret)
                               :maximizing x :into m.x
                               :counting t :into i :of-type index-type
                               :when (= x x0)
                               :do (progn
                                     (setf midx (max midx m.x))
                                     (return (values i ret)))))))
                      (cycle-walk (cyc ignore)
                        ;; Finds all cycles
                        (let ((x0 (find-if-not #'(lambda (x) (member x ignore)) arr)))
                          (if (null x0)
                              cyc
                              (multiple-value-bind (clen clst) (find-cycle x0)
                                (declare (type index-type clen)
                                         (type list clst))
                                (cycle-walk
                                 (if (= clen 0) cyc
                                     (cons (make-array clen :element-type 'index-type :initial-contents clst) cyc))
                                 (nconc ignore (if (= clen 0) (list x0) clst))))))))
               (without-tensor-safety (make-instance 'permutation-cycle :store (cycle-walk nil nil) :size (1+ midx))))))

(defmethod copy! ((act permutation-action) (type (eql 'permutation-pivot-flip)))
  (lety* ((size (permutation-size act) :type index-type)
               (actr (store act) :type index-store-vector)
               (ret (idxn size) :type index-store-vector)
               (inv (idxn size) :type index-store-vector)
               (for (idxn size) :type index-store-vector))
              (locally (declare (optimize (speed 3) (safety 0)))
               (loop :for i :of-type index-type :from 0 :below size
                     :do (let ((flip (aref inv (aref actr i))))
                           (setf (aref ret i) flip
                                 (aref inv (aref for i)) flip
                                 (aref for flip) (aref for i)))))
              (without-tensor-safety (make-instance 'permutation-pivot-flip :store ret :size size))))

(defmethod copy! ((act permutation-action) (type (eql 'permutation-action)))
  (without-tensor-safety
      (make-instance 'permutation-action :store (copy-seq (store act)) :size (permutation-size act))))
;;
(defmethod copy! ((cyc permutation-cycle) (type (eql 'permutation-action)))
  (lety ((act-repr (idxn (permutation-size cyc)) :type index-store-vector)
              (cycs (store cyc)))
             (locally (declare (optimize (speed 3) (safety 0)))
              (loop :for cyc :of-type index-store-vector :in cycs
                    :do (apply-cycle! act-repr cyc)))
             (without-tensor-safety (make-instance 'permutation-action :store act-repr :size (length act-repr)))))

(defmethod copy! ((cyc permutation-cycle) (type (eql 'permutation-pivot-flip)))
  (tensor-copy (tensor-copy cyc 'permutation-action) 'permutation-pivot-flip))

(defmethod copy! ((cyc permutation-cycle) (type (eql 'permutation-cycle)))
  (without-tensor-safety (make-instance 'permutation-cycle :store (mapcar #'copy-seq (store cyc)) :size (permutation-size cyc))))
;;
(defmethod copy! ((pflip permutation-pivot-flip) (type (eql 'permutation-action)))
  (lety* ((idiv (store pflip) :type index-store-vector)
               (len (permutation-size pflip) :type index-type)
               (ret (idxn len) :type index-store-vector))
              (without-tensor-safety (make-instance 'permutation-action :store (locally (declare (optimize (speed 3) (safety 0))) (apply-flips! ret idiv)) :size len))))

(defmethod copy! ((pflip permutation-pivot-flip) (type (eql 'permutation-cycle)))
  (tensor-copy (tensor-copy pflip 'permutation-action) 'permutation-cycle))

(defmethod copy! ((pflip permutation-pivot-flip) (type (eql 'permutation-pivot-flip)))
  (without-tensor-safety (make-instance 'permutation-pivot-flip :store (copy-seq (store pflip)) :size (permutation-size pflip))))
;;
(defun permutation/ (a)
  (etypecase a
    (permutation-action
     (lety* ((sto (store a) :type index-store-vector)
                  (rsto (t.store-allocator index-store-vector (length sto)) :type index-store-vector))
                 (loop :for i :from 0 :below (length rsto)
                       :for ele of-type index-type :across sto
                       :do (setf (aref rsto ele) i))
                 (without-tensor-safety (make-instance 'permutation-action :store rsto :size (length rsto)))))
    (permutation-cycle
     (let ((sto (store a)))
       (without-tensor-safety
           (make-instance 'permutation-cycle
             :store (loop :for cyc :of-type index-store-vector :in sto :collect (reverse cyc))
             :size (permutation-size a)))))
    (permutation-pivot-flip (tensor-copy (permutation/ (tensor-copy a 'permutation-action)) 'permutation-pivot-flip))))

(defun permutation* (a b)
  (declare (type permutation a b))
  (let ((ret (idxn (max (permutation-size a) (permutation-size b)))))
    (permute! ret b) (permute! ret a)
    (loop :for i :from (1- (length ret)) :downto 0
          :do (when (/= i (aref ret i)) (loop-finish))
          :finally (return (without-tensor-safety (make-instance 'permutation-action :store (subseq ret 0 (1+ i)) :size (1+ i)))))))
;;
(definline sort-permute (seq predicate &key (key #'identity))
  (multiple-value-bind (seq perm) (sort-index seq predicate :key key)
    (values seq (without-tensor-safety (make-instance 'permutation-action :store perm :size (length perm))))))

;;Uber-functional stuff
;;None of these are ever useful (I've found); neat things for showing off though :]
;; (defun permute-arguments-and-compile (func perm)
;;   (declare (type function func)
;;	   (type permutation perm))
;;   (let ((args (loop :for i :from 0 :below (permutation-size perm)
;;		 :collect (gensym))))
;;     (compile-and-eval `(lambda (,@args &rest rest)
;;			 (apply ,func (append (list ,@(permute! args perm)) rest))))))

;; (defun permute-arguments (func perm)
;;   (declare (type function func)
;;	   (type permutation perm))
;;   (lambda (&rest args)
;;     (apply func (permute! args perm))))

;; (defun curry (func perm &rest curried-args)
;;   (declare (type function func)
;;	   (type permutation perm))
;;   (lambda (&rest args)
;;     (apply func (permute! (append curried-args args) perm))))

;; (defun curry-and-compile (func perm &rest curried-args)
;;   (declare (type function func)
;;	   (type permutation perm))
;;   (let ((args (loop :for i :from 0 :below (permutation-size perm)
;;		 :collect (gensym))))
;;     (compile-and-eval
;;      `(let (,@(mapcar #'(lambda (a b) `(,a ,b)) args curried-args))
;;	(lambda (,@(nthcdr (length curried-args) args) &rest rest)
;;	  (apply ,func (append (list ,@(permute! args perm)) rest)))))))

;; (defun compose (func-a func-b perm)
;;   (declare (type function func-a func-b)
;;	   (type permutation perm))
;;   (lambda (&rest args)
;;     (apply func-a (permute! (multiple-value-list (funcall func-b args)) perm))))

;; (defun compose-and-compile (func-a func-b perm)
;;   (declare (type function func-a func-b)
;;	   (type permutation perm))
;;   (let ((syms (loop :for i :from 0 :below (permutation-size perm)
;;		 :collect (gensym))))
;;     (compile-and-eval
;;      `(lambda (&rest args)
;;	(destructuring-bind (,@syms &rest rest) (multiple-value-list (apply ,func-b args))
;;	  (apply ,func-a (append (list ,@(permute! syms perm)) rest)))))))

