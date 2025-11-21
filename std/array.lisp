;;; std/array.lisp --- Standard Arrays

;;

;;; Code:
(in-package :std/array)

;; NOTE 2025-04-27: probably not a good idea
(declaim (optimize (safety 0) (speed 3)))

(declaim (ftype (function (array) array) copy-array)
         (maybe-inline copy-array))
(defun copy-array (array)
  "Make a new copy of ARRAY and return it."
  (let ((new-array
          (make-array (array-dimensions array)
                      :element-type (array-element-type array)
                      :adjustable (adjustable-array-p array)
                      :fill-pointer (and (array-has-fill-pointer-p array)
                                         (fill-pointer array)))))
    (loop for i below (array-total-size array)
          do (setf (row-major-aref new-array i)
                   (row-major-aref array i)))
    new-array))

;; from petalisp
(defun simplify-array (array)
  "Returns an array with the same shape and elements as ARRAY, but that is
guaranteed to be simple."
  (if (typep array 'simple-array)
      array
      (let ((copy (make-array (array-dimensions array)
                              :element-type (array-element-type array))))
        (loop for index below (array-total-size array) do
                 (setf (row-major-aref copy index)
                       (row-major-aref array index)))
        copy)))

(deftype signed-array-length ()
  "A (possibly negated) array length."
  '#.(let ((limit (1- array-dimension-limit)))
       `(integer ,(- limit) ,limit)))

;; From Shinmera's PIPING
(defun array-shift (array &key (n 1) (from 0) (to (length array)) (adjust t) (fill nil f-p))
  "Shift a subset of array elements by a specified amount.
Optionally extend the array and fill empty space with a specified element.

N      --- The amount to be moved. Positive for right-shift, negative for left-shift.
FROM   --- region start point.
TO     --- region end point.
ADJUST --- Whether to adjust the fill pointer and the array bounds.
FILL   --- If provided, any empty space will be filled with this element."
  (when (and adjust (array-has-fill-pointer-p array))
    (unless (array-in-bounds-p array (+ (fill-pointer array) n))
      (adjust-array array (+ (fill-pointer array) n)))
    (incf (fill-pointer array) n))
  (if (< 0 n)
      (progn
        (loop repeat (- to from)
              for cursor downfrom (1- to)
              do (setf (aref array (+ cursor n))
                       (aref array cursor)))
        (when f-p
          (loop repeat n
                for cursor from from below to
                do (setf (aref array cursor) fill))))
      (progn
        (loop repeat (- to from)
              for cursor from (+ from n)
              do (setf (aref array cursor)
                       (aref array (- cursor n))))
        (when f-p
          (loop repeat (- n)
                for cursor downfrom (1- to) to from
                do (setf (aref array cursor) fill)))))
  array)

(declaim (inline vector-push-extend-position vector-pop-position))
(defun vector-push-extend-position (element vector position)
  "Push the element into the specified position and shift-right to make
space. This is potentially very costly as all elements after the given
position need to be shifted as per ARRAY-SHIFT."
  (array-shift vector :from position)
  (setf (aref vector position) element)
  (fill-pointer vector))

(defun vector-pop-position (vector position)
  "Pop the element at the given position off the vector and return it.
This is potentially very costly as all elements after the given position
need to be shifted back as per ARRAY-SHIFT."
  (let ((el (aref vector position)))
    (array-shift vector :n -1 :from (1+ position))
    el))

;; Matlisp
(declaim (inline vectorify))
(defun vectorify (seq n &optional (element-type t))
  (declare (type (or vector list) seq))
  (etypecase seq
    (cons
     (let ((ret (make-array n :element-type element-type)))
       (loop for i of-type fixnum from 0 below n
             for lst = seq then (cdr lst)
             do (setf (aref ret i) (car lst))
             finally (return ret))))
    (vector
     (let ((ret (make-array n :element-type element-type)))
       (loop for i of-type fixnum from 0 below n
             for ele across seq	    
             do (setf (aref ret i) ele)
             finally (return ret))))))

(defmacro make-array-allocator (name type init &optional doc)
  "Define an allocator function with NAME which produces a vector with
element-type TYPE and default value INIT."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (definline ,name (size &optional (initial-element ,init))
       ,@(unless (null doc)
	   `(,doc))
       (make-array size :element-type ,type :initial-element initial-element))))

(definline vector-foldl (func vec)
  (declare (type vector))
  (loop
    for i of-type fixnum from 0 below (length vec)
    for ret = (aref vec 0) then (funcall func ret (aref vec i))
    finally (return ret)))

(definline vector-foldr (func vec)
  (declare (type vector))
  (loop
    for i of-type fixnum downfrom (1- (length vec)) to 0
    for ret = (aref vec (1- (length vec))) then (funcall func (aref vec i) ret)
    finally (return ret)))

(definline vector-map-foldl (func vec)
  (declare (type vector))
  (loop
    for i of-type fixnum from 0 below (length vec)
    for ret = (aref vec 0) then (funcall func (aref vec i) ret)
    do (setf (aref vec i) ret)
    finally (return (values ret vec))))

(definline vector-map-foldr (func vec)
  (declare (type vector))
  (loop
    for i of-type fixnum downfrom (1- (length vec)) to 0
    for ret = (aref vec (1- (length vec))) then (funcall func (aref vec i) ret)
    do (setf (aref vec i) ret)
    finally (return (values ret vec))))

(definline vector-max (vec)
  (declare (type vector vec))
  (loop for ele across vec
	for idx of-type fixnum = 0 then (+ idx 1)
	with max of-type fixnum = (aref vec 0)
	with max-idx of-type fixnum = 0
	do (when (> ele max)
	     (setf max ele
		   max-idx idx))
	finally (return (values max max-idx))))

(definline vector-min (vec)
  (declare (type vector vec))
  (loop for ele across vec
	for idx of-type fixnum = 0 then (+ idx 1)
	with min of-type fixnum = (aref vec 0)
	with min-idx of-type fixnum = 0
	do (when (< ele min)
	     (setf min ele
		   min-idx idx))
	finally (return (values min min-idx))))

(definline vector-eq (va vb &optional (test #'eq))
  (declare (type vector va vb))
  (let ((la (length va))
	(lb (length vb)))
    (if (/= la lb) nil
	(loop
	  for ele-a across va
	  for ele-b across vb
	  unless (funcall test ele-a ele-b)
	  do (return nil)
	  finally (return t)))))

(definline vector-to-list (va)
  (declare (type vector va))
  (loop for ele across va
	collect ele))

(definline copy-vector-to-list (va la)
  (declare (type vector va)
	   (type list la))
  (loop
    for ele across va
    for lst = la then (cdr lst)
    do (setf (car lst) ele))
  la)

;; array indexing utils
(definline modproj (i d &optional openp def)
  (cond
    ((not i) def)
    ((not d) i)
    (t (assert (if openp (<= (- (1+ d)) i d) (< (- (1+ d)) i d)) nil 'std/condition:invalid-argument 
               :reason "invalid index" :item i)
       (if (< i 0) (if (and openp (= i (- (1+ d)))) -1 (mod i d)) i))))

;;; Binary Search
;; WIP
#+nil
(eval-every
  (let ((code `(lambda (val lb ub vec &key (order #'<) (test #'=))
                 (declare (type fixnum lb ub)
                          (type vector vec))
                 (cond
                   ((or (= lb ub) (funcall order val (aref vec lb))) (values nil lb))
                   ((funcall order (aref vec (1- ub)) val) (values nil ub))
                   (t (loop :for j :of-type fixnum := (floor (+ lb ub) 2)
                            :repeat #.(ceiling (log array-dimension-limit 2))
                            :do (cond ((funcall test (aref vec j) val) (return j))
                                      ((>= lb (1- ub)) (return (values nil (if (funcall order (aref vec lb) val) (1+ lb) lb))))
                                      (t (if (funcall order val (aref vec j))
                                             (setf ub j)
                                             (setf lb (1+ j)))))))))))
    (setf (symbol-function 'binary-search) (compile nil code))
    (define-compiler-macro binary-search (&whole form val lb ub vec &key (order '(function <)) (test '(function =)))
      (let ((t-code (maptree-eki #'(lambda (x)
                                     ;; TODO <2025-11-20 Thu>
                                     (cond
                                       ((and (listp x) (= (length x) 3))
                                        (destructuring-bind (type vector vec) x
                                          (declare (ignore vector))
                                          (cond 
                                            ((and (listp vec) (eql (car vec) 'the))
                                             `(type ,type vec)) 
                                            (t x))))
                                       ((and (listp x) (eql (car x) 'funcall))
                                        (destructuring-bind (fname &rest argvs) x
                                          (let ((y (ecase fname (order order) (test test))))
                                            (cond
                                              ((and (listp y) (or (eql (car y) 'quote) (eql (car y) 'function)) (typep (second y) 'symbol)) `(,(second y) ,@argvs))
                                              ((listp y)
                                               (cond 
                                                 ((and (eql (car y) 'function) (listp (second y))
                                                       (eql (cadr y) 'lambda)
                                                       (listp (cdadr y)))
                                                  (let ((args (cadadr y))
                                                        (body (cdr (cdadr y))))
                                                    `(let (,@(zip args argvs)) ,@body)))
                                                 ((and (eql (car y) 'lambda)
                                                       (listp (second y)))
                                                  (let ((args (second y))
                                                        (body (cddr y)))
                                                    `(let (,@(zip args argvs)) ,@body)))
                                                 (t x)))
                                              (t x)))))
                                       (t (values x t))))
                                 (cddr code))))
        `(let (,@(zip (subseq (second code) 0 4) (cdr form))
               (order ,order)
               (test ,test))
           (declare (ignorable order test))
           ,@t-code))))

  (let ((code `(lambda (seq predicate &key key)
                 (declare (type vector seq))
                 (lety* ((key (or key #'identity))
                              (len (length seq) :type fixnum)
                              (perm (make-array len :element-type 'fixnum) :type (simple-array fixnum))
                              (jobs (make-array len :adjustable t :fill-pointer 0)))
                             (declare (ignorable key))
                             (loop :for i :of-type fixnum :from 0 :below (length perm) :do (setf (aref perm i) i))
                             (loop
                               :for bounds := (cons 0 len) :then (unless (zerop (length jobs)) (vector-pop jobs))
                               :until (null bounds)
                               :do (lety* ((below-idx (car bounds) :type fixnum)
                                                (above-idx (cdr bounds) :type fixnum)
                                                (piv (+ below-idx (floor (- above-idx below-idx) 2)) :type fixnum))
                                               (loop
                                                 :with ele := (funcall key (aref seq piv))
                                                 :with lbound :of-type fixnum := below-idx
                                                 :with ubound :of-type fixnum := (1- above-idx)
                                                 :until (progn
                                                          (loop :for i :of-type fixnum :from lbound :to piv
                                                                :until (or (= i piv) (funcall predicate ele (funcall key (aref seq i))))
                                                                :finally (setq lbound i))
                                                          (loop :for i :of-type fixnum :downfrom ubound :to piv
                                                                :until (or (= i piv) (funcall predicate (funcall key (aref seq i)) ele))
                                                                :finally (setq ubound i))
                                                          (cond
                                                            ((= ubound lbound piv)
                                                             (when (> (- piv below-idx) 1)
                                                               (vector-push-extend (cons below-idx piv) jobs))
                                                             (when (> (- above-idx (1+ piv)) 1)
                                                               (vector-push-extend (cons (1+ piv) above-idx) jobs))
                                                             t)
                                                            ((< lbound piv ubound)
                                                             (rotatef (aref seq lbound) (aref seq ubound))
                                                             (rotatef (aref perm lbound) (aref perm ubound))
                                                             (incf lbound) (decf ubound)
                                                             nil)
                                                            ((= lbound piv)
                                                             (rotatef (aref seq piv) (aref seq (1+ piv)))
                                                             (rotatef (aref perm piv) (aref perm (1+ piv)))
                                                             (unless (= ubound (1+ piv))
                                                               (rotatef (aref seq piv) (aref seq ubound))
                                                               (rotatef (aref perm piv) (aref perm ubound)))
                                                             (incf piv) (incf lbound)
                                                             nil)
                                                            ((= ubound piv)
                                                             (rotatef (aref seq (1- piv)) (aref seq piv))
                                                             (rotatef (aref perm (1- piv)) (aref perm piv))
                                                             (unless (= lbound (1- piv))
                                                               (rotatef (aref seq lbound) (aref seq piv))
                                                               (rotatef (aref perm lbound) (aref perm piv)))
                                                             (decf piv) (decf ubound)
                                                             nil)))))
                               :finally (return (values seq perm)))))))
    (setf (symbol-function 'sort-index) (compile nil code)
          (documentation 'sort-index 'function)
          "
  Sorts a lisp-vector in-place, by using the function @arg{predicate} as the
  order. Also computes the permutation action which would sort the original
  sequence @arg{seq} when applied.")
    (define-compiler-macro sort-index (&whole form seq predicate &key key)
      (let ((t-code (maptree-eki #'(lambda (x)
                                     (match x
                                       ('(type vector seq) (match seq ((list 'the type _) `(type ,type seq)) (_ x)))
                                       ((list* 'funcall 'predicate argvs)
                                        (match predicate
                                          ((list (or 'function 'quote) (and f (type symbol))) `(,f ,@argvs))
                                          ((or (list 'function (list* 'lambda (and args (list _ _)) body)) (list* 'lambda (and args (list _ _)) body))
                                           `(let (,@(zip args argvs)) ,@body))
                                          (_ x)))
                                       ((list* 'funcall 'key argvs)
                                        (match (or key '(lambda (x) x))
                                          ((list (or 'function 'quote) (and f (type symbol))) `(,f ,@argvs))
                                          ((or (list 'function (list* 'lambda (and args (list _)) body)) (list* 'lambda (and args (list _)) body))
                                           `(let (,@(zip args argvs)) ,@body))
                                          (_ x)))
                                       (_ (values x t))))
                                 (cddr code))))
        `(let ((seq ,seq)
               (predicate ,predicate)
               (key ,key))
           (declare (ignorable predicate key))
           ,@t-code)))))
