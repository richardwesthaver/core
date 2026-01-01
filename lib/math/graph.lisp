;;; graph.lisp --- Graph Algorithms

;; Tensor-based Graph Algos

;;; Commentary:

;; The algorithms in this file are derived from the MATLISP source code.

;; uses FIB-HEAP from STD/SEQ, based on DLIST from STD/LIST

;;; Code:
(in-package :math/graph)

;; required tensor method for FIB-HEAP struct
(defmethod total-size ((self fib-heap)) (slot-value self 'std/seq::elements))

(define-tensor-generic graph-to-adlist (g))

;; FIX 2025-12-31: need to take another look at the in-vector iterator for
;; correct from/below behavior
(define-tensor-method graph-to-adlist ((g graph-accessor :x))
  `(with-memoization ()
     (loop for i from 0 below (1- (length (memoizing (fence g) :type index-store-vector)))
           with ret = (make-array (1- (length (memoizing (fence g)))) :initial-element nil)
           do (loop for j across (memoizing (δ-i g) :type index-store-vector) from (aref (memoizing (fence g)) i) below (aref (memoizing (fence g)) (1+ i)) 
                    for m = 0 then (incf m)
                    do (push ,@(if (subtypep (cl :x) 'tensor) `((cons j (t.store-ref ,(cl :x) (memoizing (t.store ,(cl :x) g) :type ,(store-type (cl :x))) m))) `(j)) (aref ret i)))
           finally (return ret))))

(defun adlist-to-graph (ag &optional type &aux (type (or type 'graph-accessor)))
  (lety* ((ag (coerce ag 'vector))
          (ret (zeros (list (length ag) (length ag)) type (loop for ai across ag summing (length ai)))
               :type graph-accessor))
    (with-memoization ()
      (loop for i from 0 below (length ag)
            initially (setf (aref (memoizing (fence ret) :type index-store-vector) 0) 0)
            do (setf (aref (memoizing (fence ret)) (1+ i)) (aref (memoizing (fence ret)) i))
            do (loop for u in (setf (aref ag i) (sort (aref ag i) #'< :key #'(lambda (x) (etypecase x (cons (the index-type (first x))) (index-type x)))))
                     do (letv* ((u/ value (etypecase u (cons (the index-type (values (first u) (cdr u)))) (index-type u)))
                                (m (aref (memoizing (fence ret)) (1+ i))))
                          (setf (aref (memoizing (δ-i ret) :type index-store-vector) m) (modproj u/ (length ag) nil))
                          (if value (setf (store-ref (the graph-tensor ret) m) value)))
                     do (incf (aref (memoizing (fence ret)) (1+ i))))))
    ret))

(defun hyper->bipartite (hh &optional type full)
  (letv* ((vv (coerce (sort (reduce #'union hh) #'<) 'index-store-vector)) (hh (coerce hh 'vector))
          (n (length vv)) (m (length hh)))
    (if full
        (let ((hh (symmetrize! (concatenate 'vector (make-array n :initial-element nil) hh))))
          (adlist-to-graph hh))
        (let ((ret (zeros (list n m) (or type 'graph-accessor) (loop for h across hh summing (length h)))))
          (loop for i from 0 below (length hh)
                do (setf (aref hh i) (sort (aref hh i) #'<))
                do (loop for u in (aref hh i)
                         do (setf (aref (δ-i ret) (+ (fence ret i) j)) u)
                         counting t into j
                         finally (setf (aref (fence ret) (1+ i)) (+ (fence ret i) j))))
          ret))))

(defun order-to-tree (order &optional type)
  (adlist-to-graph
   (symmetrize!
    (coerce
     (loop for i from 0 below (length order)
           collect (remove-duplicates (list i (aref order i))))
     'vector))
   type))

(defun order-to-dag (order &optional type)
  (adlist-to-graph
   (coerce
    (loop for i from 0 below (length order)
          collect (remove-duplicates (list i (aref order i))))
    'vector)
   type))

(defun cliquep (g lst)
  (loop named main for u* on lst
        do (loop for v in (cdr u*) do (or (δ-i g (car u*) v) (return-from main nil)))
        finally (return-from main t)))

(defun gnp (n p)
  ;;TODO: Implement fast version from "Efficient generation of large random networks" - V. Batagelj, U. Brandes, PRL E 71
  ;;Current alg is O(n^2) and is way too slow.
  (let ((ret (zeros (list n n) (tensor 'index-type 'hash-tensor))))
    (loop for i from 0 below n
          do (loop for j from (1+ i) below n
                   when (< (random 1d0) p)
                   do (setf (ref ret i j) 1
                            (ref ret j i) 1)))
    (tensor-copy ret '(index-type graph-accessor))))

;;Oh may we weep for sins
(defun moralize! (adg)
  (let ((cadg (tensor-copy adg)))
    (loop for u from 0 below (length adg)
          do (let ((pa (remove-if #'(lambda (x) (find u (aref adg x))) (aref adg u))))
               (loop for p.i in pa do (setf (aref cadg p.i) (union (aref cadg p.i) (list* u pa))))))
    cadg))

(defun symmetrize! (adg)
  (loop for u from 0 below (length adg)
        do (loop for v in (aref adg u)
                 do (setf (aref adg v) (union (aref adg v) (list u)))))
  adg)

(defun graph-queue (init g)
  (declare (type graph-accessor g))
  (let* ((queue (make-heap #'(lambda (a b) (if (and a b) (< a b) (and a t))))))
    (loop for i from 0 below (1- (length (fence g))) do (fib-insert (funcall init g i) queue))
    queue))

(defmacro graphfib ((g graph &key order iterate block-name) init update &rest body)
  (with-gensyms (fe queue)
    (destructuring-bind (init-sym (i) &rest init-body) init
      (assert (eql init-sym :init) nil "key mismatch.")
      (destructuring-bind (update-sym (j w-j fib) &rest update-body) update
        (assert (eql update-sym :update) nil "key mismatch.")
        `(block ,block-name
           (lety* ((,g ,graph :type graph-accessor)
                   (,fe (fence ,g) :type index-store-vector)
                   (,fib (let* ((,queue (make-heap ,(or order #'(lambda (a b) (if (and a b) (< a b) (and a t)))))))
                           (loop for ,i from 0 below (1- (length (fence ,g)))
                                    do (fib-insert (progn ,@init-body) ,queue))
                           ,queue)))
             (loop until (= (total-size fib) 0)
                      do (progn
                           ,@(when iterate
                               (letv* (((lvar ldir) iterate))
                                 `((for ,lvar initially ,@(ecase ldir (:up `(0 then (1+ ,lvar))) (:down `((- (length ,fe) 2) then (1- ,lvar))))))))
                           (letv* ((,w-j ,j (extract-min ,fib) :type t index-type))
                             ,@update-body)))
             ,@body))))))

;;TODO: The clique-check can be eliminated, apparently. See Tarjan's paper.
#+nil
(defun max-cardinality-search (g &optional start)
  (let* ((order (tensor::t.store-allocator index-store-vector (1- (length (fence g)))))
         (start (or start (random (length order))))
         (cliques nil)
         (k (1- (length (fence g)))) (stack nil))
    (graphfib (g g :order (lambda (x y) (> x y)))
      (:init (i) (if (= i start) 1 0))
      (:update (i w-i fib)
         (letv* ((li ri (fence g i))
                 (δ-clique (loop for j across (δ-i g) from li below ri 
                                 when (or (member j stack) (std/seq::node-existsp j fib))
                                 collect j)))
           (if (cliquep g δ-clique)
               (loop for j across (δ-i g) from li below ri
                     do (incf (std/seq::node-key j fib))
                     finally 
                        (progn 
                          (setf (aref order (decf k)) i)
                          (setf cliques (let ((c (list (cons i δ-clique)))) (union cliques (union c cliques :test #'subsetp) :test #'subsetp)))
                          (loop for u in stack do (fib-insert (std/seq::node-key u fib) fib u)
                                finally (setf stack nil))))
               (push i stack))))
      (unless stack (values (reverse order) cliques)))))

;;; Triangulate
;; (graph-to-adlist (adlist-to-graph (symmetrize! #((1) (2) (0 3) (4) (0)))))
;; (symmetrize! (coerce (append (mapcar #'list (range 1 10 1 t)) (list '(0))) 'vector))
