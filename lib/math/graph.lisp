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
           do (loop for j across (subseq (memoizing (δ-i g) :type index-store-vector) (aref (memoizing (fence g)) i) (aref (memoizing (fence g)) (1+ i)))
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
(defun max-cardinality-search (g &optional start)
  (let* ((order (tensor::t.store-allocator index-store-vector (1- (length (fence g)))))
         (start (or start (random (length order))))
         (cliques nil)
         (k (1- (length (fence g)))) (stack nil))
    (graphfib (g g :order (lambda (x y) (> x y)))
      (:init (i) (if (= i start) 1 0))
      (:update (i w-i fib)
         (letv* ((li ri (fence g i))
                 (δ-clique (loop for j across (subseq (δ-i g) li ri)
                                 when (or (member j stack) (std/seq::node-existsp j fib))
                                 collect j)))
           (if (cliquep g δ-clique)
               (loop for j across (subseq (δ-i g) li ri)
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
;;Naive-implementation, can't use graphfib because of non-monotonicity
;;Use union-find/hash-table in place of list forc sets.
(defun triangulate-graph (g &optional heuristic)
  (let* ((ag (graph-to-adlist g)) (heuristic (or heuristic :min-fill))
         (ord (tensor::t.store-allocator index-store-vector (length ag))))
    (flet ((cliquify (u)
             (loop for v in (aref ag u)
                   do (setf (aref ag v) (set-difference (aref ag v) (list u v))))
             (setf (aref ag u) t))
           (δ-size (i) (length (aref ag i)))
           (k-size (i) (loop for u* on (aref ag i)
                             with ret
                             do (loop for v in (cdr u*)
                                      unless (find (car u*) (aref ag v))
                                      counting t into ret)
                             finally (return ret))))
      (loop for i from 0 below (length ord)
            do (setf (aref ord i) (loop for i from 0 below (length ag)
                                        unless (eql (aref ag i) t)
                                        minimizing (ecase heuristic (:min-fill (δ-size i)) (:min-size (k-size i)))
                                        return i))
            do (cliquify (aref ord i))))
    ord))

;;Translated from Tim Davis' CSparse
(defun elimination-tree (order g)
  (declare (type graph-accessor g))
  (let ((iord (t.store-allocator index-store-vector (length order)))
        (ancestor (t.store-allocator index-store-vector (length order) :initial-element -1))
        (parent (t.store-allocator index-store-vector (length order) :initial-element -1)))
    (declare (type index-store-vector iord ancestor parent))
    (loop for i from 0 below (length iord) do (setf (aref iord (aref order i)) i))
    (loop for u across order
          do (setf (aref parent u) u)
          do (letv* ((ll rr (fence g (the index-type u)) :type index-type index-type))
               (loop for v across (subseq (δ-i g) ll rr)
                     when (< (aref iord v) (aref iord u))
                     do (loop for h = v then (let ((h+ (aref ancestor h))) 
                                               (setf (aref ancestor h) u)
                                               (when (or (< h+ 0) (= h+ u)) (setf (aref parent h) u) (loop-finish))
                                               h+)))))
    (values parent iord)))

;;Translated from Tim Davis' CSparse
(defun cholesky-cover (g order)
  (declare (type graph-accessor g)
           (type index-store-vector order))
  (letv* ((etree iord (elimination-tree order g)) (color (t.store-allocator #.(tensor 'boolean) (length etree)))
          (adj (make-array (length etree) :initial-element nil)))
    (macrolet ((refc (x i) `(t.store-ref #.(tensor 'boolean) ,x ,i)))
      (loop for u across order 
            do (setf (refc color u) t) 
            do (push u (aref adj u))
            do (letv* ((ll rr (fence g u)))
                 (loop for v across (subseq (δ-i g) ll rr)
                       for iuv from 0 below (- rr ll)
                       when (< (aref iord v) (aref iord u))
                       do (loop for w = v then (aref etree w)
                                if (refc color w) do (loop-finish)
                                else do (progn (setf (refc color w) t)
                                               (push w (aref adj u)))))
                 (loop for v in (aref adj u) do (setf (refc color v) nil)))))
    (let ((lg (adlist-to-graph adj (type-of g))))
      #+nil
      (iter (for u from 0 below (1- (length (fence g))))
            (letv* ((ll rr (fence g u)))
              (iter (for v in-vector (δ-i g) from ll below rr with-index iuv)
                    (when (< (aref iord v) (aref iord u)) (setf (ref lg v u) (store-ref g iuv))))))
      (values lg iord))))

(defun chordal-cover (g order &optional type)
  (declare (type graph-accessor g)
           (type index-store-vector order))
  (let* ((cc (graph-to-adlist g))
         (vs (make-array (length cc) :initial-element nil)))
    (loop for i across order
          do (loop for j in (aref cc i)
                   unless (aref vs j)
                   do (setf (aref cc j) (union (aref cc j) (remove-if #'(lambda (x) (or (= x j) (aref vs x))) (aref cc i)))))
          do (setf (aref vs i) t))
    (adlist-to-graph cc type)))

(defun line-graph (hh)
  (letv* ((hh (coerce hh 'vector)) (m (length hh))
          (ret (zeros (list m m) (tensor t 'hash-tensor))))
    (loop for i from 0 below (length hh)
          do (loop for j from (1+ i) below (length hh)
                   do (when-let ((int (intersection (aref hh i) (aref hh j))))
                        (setf (ref ret i j) int
                              (ref ret j i) int))))
    (copy ret '(t graph-accessor))))

;;; Dijkstra
(defun dijkstra (g &optional start)
  (declare (type graph-accessor g))
  (let* ((tree (t.store-allocator index-store-vector (dimensions g 0)))
         (start (or start (random (length tree)))))
    (setf (aref tree start) start)
    (graphfib (g g :order (lambda (x y) (if (and x y) (< x y) (and x t))))
      (:init (i) (if (= i start) 0 nil))
      (:update (i d-i fib)
         (letv* ((li ri (fence g i)))
           (loop for j across (subseq (δ-i g) li ri)
                 when (std/seq::node-existsp j fib)
                 do (let ((d-j+ (+ d-i (if (typep g 'base-tensor) (ref g i j) 1))) (k-j (std/seq::node-key j fib)))
                      (when (or (not k-j) (< d-j+ k-j))
                        (setf (std/seq::node-key j fib) d-j+
                              (aref tree j) i))))))
      tree)))

(defun dijkstra-prims (g &optional start)
  (declare (type graph-accessor g))
  (let* ((tree (t.store-allocator index-store-vector (dimensions g 0)))
         (start (or start (random (length tree)))))
    (loop for i from 0 below (length tree) do (setf (aref tree i) i))
    (graphfib (g g :order (lambda (x y) (if (and x y) (< x y) (and x t))))
      (:init (i) (if (= i start) 0 nil))
      (:update (i w-i fib)
         (letv* ((li ri (fence g i)))
           (loop for j across (subseq (δ-i g) li ri)
                 when (std/seq::node-existsp j fib)
                 do (let ((w-ij (if (typep g 'base-tensor) (ref g i j) 1)) (k-j (std/seq::node-key j fib)))
                      (when (or (not k-j) (< w-ij k-j))
                        (setf (std/seq::node-key j fib) w-ij
                              (aref tree j) i))))))
      tree)))

(defun tree-decomposition (g &optional type heuristic)
  (letv* ((cliques (or (nth-value 1 (max-cardinality-search g)) (nth-value 1 (max-cardinality-search (chordal-cover g (triangulate-graph g heuristic))))))
          (k (length cliques)))
    (values
     (let ((ret (zeros (list k k) (tensor 'index-type 'hash-tensor))))
       (loop for cc on cliques
             do (loop for cp in (cdr cc)
                      counting t into j
                      do (when-let ((int (intersection (car cc) cp)))
                           (setf (ref ret i (+ i j)) (- (length int))
                                 (ref ret (+ i j) i) (ref ret i (+ i j)))))
             counting t into i)
       (order-to-tree (dijkstra-prims (copy ret (tensor 'index-type 'simple-graph-tensor))) type))
     (coerce cliques 'vector))))

(defun directed-subgraph (g)
  (let ((adg (graph-to-adlist g)))
    (loop for u from 0 below (length adg) do (setf (aref adg u) (remove-if #'(lambda (x) (declare (type index-type x u)) (δ-i g x u)) (aref adg u))))
    (adlist-to-graph adg (class-of g))))

;; 1/2 approximation,
(defun max-dag (g)
  "1/2 approximation to the Maximum acyclic subgraph problem (anything better is NP-hard assuming UGC)."
  (let* ((g (directed-subgraph g)) (gt (transpose g))
         (adg (make-array (dimensions g -1) :initial-element nil)))
    (graphfib (g g :order #'(lambda (a b) (< (first a) (first b))))
      (:init (i) (list (δ-i g i :size) (δ-i gt i :size)))
      (:update (i d-i fib)
         (map nil #'(lambda (v) (when (std/seq::node-existsp v fib)
                                  (letv* (((a b) (std/seq::node-key v fib)))
                                    (setf (std/seq::node-key v fib) (list (1- a) b))))) (δ-i gt i t))
         (map nil #'(lambda (v) (when (std/seq::node-existsp v fib)
                                  (letv* (((a b) (std/seq::node-key v fib)))
                                    (setf (std/seq::node-key v fib) (list a (1- b)))))) (δ-i g i t))
         (if (>= (first d-i) (second d-i))
             (map nil #'(lambda (v) (if (std/seq::node-existsp v fib) (pushnew v (aref adg i)))) (δ-i g i t))
             (map nil #'(lambda (v) (if (std/seq::node-existsp v fib) (pushnew i (aref adg v)))) (δ-i gt i t))))
      (adlist-to-graph adg (type-of g)))))

(defun topological-order (dag)
  (let ((dagt (transpose dag))
        (order (t.store-allocator index-store-vector (dimensions dag -1)))
        (visited (make-array (dimensions dag -1) :element-type 'boolean :initial-element nil)))
    (loop named outer for cu across visited for u from 0 below (length visited)
          with ii = -1
          unless cu
          ;; FIX 2026-01-01: WITH-COLOR
          do (loop for tu being the gidx of dag from u in-order :sfd
                      #+nil (with-color color with-parent tp with-visited-array visited)
                   do (setf (aref order (incf ii)) tu)
                   when (some #'(lambda (x) (aref color x)) (δ-i dagt tu t))
                   do (return-from outer))
          finally (return-from outer order))))
