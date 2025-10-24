;;; lib/obj/graph.lisp --- Graphs

;; Graph objects and algorithms

;;; Commentary:

;; Modeled off of eschulte's GRAPH library - see also DAT/DOT

;; ref: https://eschulte.github.io/graph/

;; Our goals are slightly different than the original library - we prioritize
;; flexibility over speed or code size and generally prefer vectors to
;; hash-tables. To this end, we support either collection type as the values
;; of the NODES and EDGES slots of the GRAPH class. Mix and match - no
;; problemo.

;; Eschulte's graph library only supports symbols and integers as node values
;; (keys must be EQUAL-safe), our implementation supports strings as well, and
;; supports Eschulte's graph API. Note that the intended use-case here - when
;; using simple objects as node values + hash-tables, is that you are
;; supplying 'pointers' to objects which exist elsewhere - it is assumed that
;; you are encoding as little 'information' into your graph directly and
;; instead using it to perform complex calculations and transformations using
;; the 'shape' which your graph's nodes and edges imply.

;; Nowadays we prefer CLOS objects where possible. NODE is a class from them
;; OBJ/AST module which has a single AST slot and serves as the basis for both
;; the VERTEX and EDGE classes in this module. A VERTEX is a KV pair where
;; K=ID and V=AST. The AST slot may be used as the EDGE-VALUE (property list,
;; etc) of the EDGE class, which also includes IN and OUT slots. In Graphviz
;; parlance the EDGE class is 'undirected' (--), the DIRECTED-EDGE class
;; represents the 'directed' variant (->). Finally we supply a default
;; WEIGHTED-EDGE which contains an additional WEIGHT slot.

;; If you care about direction, use DIRECTED-GRAPH instead of GRAPH.


;;; Code:
(in-package :obj/graph)

(in-readtable :std)

;;; Vertex
(defclass vertex (id node)
  ()
  (:documentation "generic vertex mixin. The difference between this class and NODE is
that a vertex always carries an ID slot."))

;;; Edge
(defclass edge (node)
  ((in :initarg :in :accessor edge-in) (out :initarg :out :accessor edge-out))
  (:documentation "generic edge mixin. Compatible with the NODE and ID protocols."))

(defmethod name ((self edge))
  (cons (edge-in self) (edge-out self)))

(defclass edgex (edge id)
  ()
  (:documentation "Edge compatible with the NODE and ID protocols."))

(defclass directed-edge (edge)
  ()
  (:documentation "An edge with an implicit direction from IN to OUT."))

(defclass weighted-edge (edge)
  ((weight :initform 1d0 :initarg :weight :accessor weight-of)))

;;; Hashing
;; despite preferring vectors, we provide support for custom hashers for
;; users. These functions support ESCHULTE's API only.
(defun node-hash-equal (hash1 hash2)
  "Test node hashes HASH1 and HASH2 for equality."
  (set-equal (hash-table-alist hash1)
             (hash-table-alist hash2)
             :test (lambda (a b)
                     (or (and (atom a) (atom b) (equalp a b))
                         (and (equalp (car a) (car b))
                              (set-equal (cdr a) (cdr b) :test 'tree-equal))))))

(defun edge-hash-equal (hash1 hash2)
  "Test edge hashes HASH1 and HASH2 for equality."
  (set-equal (hash-table-alist hash1)
             (hash-table-alist hash2)
             :test 'equalp))

(defun edge-equalp (edge1 edge2)
  (when (atom edge1) (setf edge1 (list edge1)))
  (when (atom edge2) (setf edge2 (list edge2)))
  (set-equal (flatten edge1) (flatten edge2) :test 'equal))

(defun directed-edge-equalp (edge1 edge2)
  (tree-equal edge1 edge2))

(defun sxhash-edge (edge)
  (sxhash 
   (if (atom edge)
       edge
       (sort
        (flatten (copy-tree edge))
        (cond
          ((and (numberp (car edge)) (numberp (cdr edge)))
           (lambda (a b)
             (or (< (imagpart a) (imagpart b))
                 (and (= (imagpart a) (imagpart b))
                      (< (realpart a) (realpart b))))))
          ((and (stringp (car edge)) (stringp (cdr edge)))
           #'string<)
          (t
           (lambda (a b) (declare (ignore a b)) t)))))))

(sb-ext:define-hash-table-test edge-equalp sxhash-edge)

(sb-ext:define-hash-table-test directed-edge-equalp sxhash)

;;; Proto
(defgeneric nodes (graph))
(defgeneric (setf nodes) (graph nodes))
(defgeneric edges (graph))
(defgeneric (setf edges) (graph edges))

(defgeneric graph-equal (graph1 graph2))

(defgeneric subgraph (graph nodes)
  (:documentation "Return the subgraph of GRAPH restricted to NODES."))

(defgeneric delete-node (graph node)
  (:documentation "Delete NODE from GRAPH.
Delete and return the old edges of NODE in GRAPH."))

(defgeneric has-node-p (graph node)
  (:documentation "Return non-nil if GRAPH has node NODE."))
(defgeneric has-edge-p (graph edge)
  (:documentation "Return `true' if GRAPH has edge EDGE."))

(defgeneric edge-weight (edge &key &allow-other-keys)
  (:method ((edge t) &key &allow-other-keys) (values 1.0)))

(defgeneric edge-value (graph edge)
  (:method ((graph t) (edge t)) (values nil)))

(defgeneric (setf edge-value) (new graph edge))

(defgeneric delete-edge (graph edge)
  (:documentation "Delete EDGE from GRAPH.
Return the old value of EDGE."))

(defgeneric node-edges (graph node)
  (:documentation "Return the edges of NODE in GRAPH."))

(defgeneric (setf node-edges) (new graph node)
  (:documentation "Set the edges of NODE in GRAPH to NEW.
Delete and return the old edges of NODE in GRAPH."))

(defgeneric add-node (graph node))

(defgeneric add-edge (graph edge &optional value))

;;; Graph
(defclass graph (node)
  ((nodes :initform (make-hash-table :test 'equal)
          :type (or (vector node) hash-table)
          :accessor nodes
          :initarg :nodes)
   (edges :initform (make-hash-table :test 'edge-equalp)
          :type (or (vector edge) hash-table)
          :accessor edges
          :initarg :edges))
  (:documentation "generic graph object."))

(defmethod copy-graph ((graph graph))
  (make-instance (type-of graph) :nodes (copy-object (nodes graph)) :edges (copy-object (edges graph))))

(defmethod copy-object ((graph graph))
  (copy-graph graph))

(defmethod subgraph ((graph graph) nodes)
  (make-instance (type-of graph) :nodes nodes :edges (copy-object (edges graph))))

(defmethod has-edge-p ((graph graph) edge)
  (multiple-value-bind (value included) (get-val (edges graph) edge)
    (declare (ignorable value)) included))

(defmethod has-node-p ((graph graph) node)
  (multiple-value-bind (value included) (get-val (nodes graph) node)
    (declare (ignorable value)) included))

(defmethod delete-node ((graph graph) node)
  (prog1 (mapcar (lambda (edge) (cons edge (delete-edge graph edge)))
                 (node-edges graph node))
    (remhash node (nodes graph))))

(defmethod delete-edge ((graph graph) edge)
  (prog1 (edge-value graph edge)
    (mapc (lambda (node) (setf (get-val (nodes graph) node)
                          (remove edge (get-val (nodes graph) node)
                                  :test 'edge-equalp)))
          edge)
    (remhash edge (edges graph))))

(defmethod node-edges ((graph graph) node)
  (multiple-value-bind (edges included) (get-val (nodes graph) node)
    (assert included (node graph) "~S doesn't include ~S" graph node)
    edges))

(defmethod (setf node-edges) (new (graph graph) node)
  (prog1 (mapc {delete-edge graph} (get-val (nodes graph) node))
    (mapc {add-edge graph} new)))

(defmethod add-edge ((graph graph) (edge list) &optional value)
  (mapc (lambda (node)
          (when (hash-table-p (nodes graph))
            (add-node graph node))
          (pushnew (case (type-of graph)
                     (graph (remove-duplicates edge))
                     (directed-graph edge))
                   (get-val (nodes graph) node)
                   :test 'edge-equalp))
        edge)
  (setf (get-val (edges graph) edge) value)
  edge)

(defmethod add-edge ((graph graph) (edge edge) &optional value)
  (dolist (n (list (edge-in edge) (edge-out edge)))
    (when (hash-table-p (nodes graph))
      (add-node graph n))
    (when (hash-table-p (nodes graph))
      (pushnew (case (type-of graph)
                 (graph (remove-duplicates edge))
                 (directed-graph edge))
               (get-val (nodes graph) n)
               :test 'edge-equalp)))
  (setf (get-val (edges graph) (name edge)) (or value edge))
  edge)

(defmethod add-edge ((graph graph) (edge id) &optional value)
  (add-edge graph (id edge) (or value edge)))

(defmethod edge-value ((graph graph) edge)
  (name (get-val (edges graph) edge :key 'id)))

(defmethod (setf edge-value) (new (graph graph) edge)
  (etypecase (edges graph)
    (hash-table (setf (get-val (edges graph) edge) new))
    (sequence (setf (nth (position edge (edges graph) :key 'id:id) (edges graph)) new))))

(defgeneric merge-nodes (graph node1 node2 &key new)
  (:documentation "Combine NODE1 and NODE2 in GRAPH into the node NEW.
All edges of NODE1 and NODE2 in GRAPH will be combined into a new node
of value NEW.  Edges between only NODE1 and NODE2 will be removed."))

(defmethod merge-nodes ((graph graph) node1 node2 &key (new node1))
  ;; replace all removed edges with NEW instead of NODE1 or NODE2
  (mapcar
   (lambda (l)
     (destructuring-bind (edge . value) l
       (let ((e (mapcar (lambda (n) (if (member n (list node1 node2)) new n)) edge)))
         (if (has-edge-p graph e)
             (when (and (edge-value graph e) value)
               (setf (edge-value graph e) (+ (edge-value graph e) value)))
             (add-edge graph e value)))))
   ;; drop edges between only node1 and node2
   (remove-if-not [{set-difference _ (list node1 node2)} #'car]
                  ;; delete both nodes keeping their edges and values
                  (prog1 (append (delete-node graph node1)
                                 (delete-node graph node2))
                    ;; add the new node
                    (add-node graph new))))
  graph)

(defgeneric merge-edges (graph edge1 edge2 &key value)
  (:documentation "Combine EDGE1 and EDGE2 in GRAPH into a new EDGE.
Optionally provide a value for the new edge, the values of EDGE1 and
EDGE2 will be combined."))

(defmethod merge-edges ((graph graph) edge1 edge2 &key value)
  (add-edge graph (remove-duplicates (append edge1 edge2))
            (or value
                (when (and (edge-value graph edge1) (edge-value graph edge2))
                  (+ (edge-value graph edge1) (edge-value graph edge2)))))
  (append (delete-edge graph edge1)
          (delete-edge graph edge2)))

(defgeneric degree (graph node)
  (:documentation "Return the degree of NODE in GRAPH."))

(defmethod degree ((graph graph) node)
  (length (node-edges graph node)))

(defmethod add-node ((graph graph) node)
  (assert (or (numberp node) (symbolp node) (stringp node)) (node)
          "Nodes must be numbers, symbols, strings or keywords, not ~S.~%Invalid node:~S"
          (type-of node) node)
  (unless (and (hash-table-p (nodes graph)) (has-node-p graph node))
    (setf (get-val (nodes graph) node) nil)
    node))

(defmethod add-node ((graph graph) (node id))
  (add-node graph (id node)))

;;; Directed Graph
(defclass directed-graph (graph)
  ((edges :initform (make-hash-table :test 'directed-edge-equalp)
          :type (or (vector directed-edge) hash-table)
          :accessor edges
          :initarg :edges))
  (:documentation "graph with only directed edges."))

(defgeneric indegree (digraph node)
  (:documentation "The number of edges directed to NODE in GRAPH."))

(defmethod indegree ((digraph directed-graph) node)
  (length (remove-if-not [{member node} #'cdr] (node-edges digraph node))))

(defgeneric outdegree (digraph node)
  (:documentation "The number of edges directed from NODE in DIGRAPH."))

(defmethod outdegree ((digraph directed-graph) node)
  (length (remove-if-not [{equal node} #'car] (node-edges digraph node))))

;;; Shortest Path
(defgeneric shortest-path (graph a b &optional heuristic)
  (:documentation "Return the shortest path in GRAPH from A to B.
Implemented using A* search.  Optional argument HEURISTIC may be a
function which returns an estimated heuristic cost from an node to the
target B.  The default value for HEURISTIC is the constant function of
0, reducing this implementation to Dijkstra's algorithm.  The
HEURISTIC function must satisfy HEURITIC(x)≤d(x,y)+HEURITIC(y) ∀ x,y
in GRAPH allowing the more efficient monotonic or 'consistent'
implementation of A*.")
  (:method ((graph graph) a b
            &optional
              (heuristic (constantly 0))
            &aux
              (from (make-hash-table))
              (fringe (sb-concurrency:make-queue))
              (open (make-hash-table))
              (closed (make-hash-table))
              (g (make-hash-table))
              (f (make-hash-table)))
    (when (equal a b) (return-from shortest-path nil))
    (labels ((reconstruct-path (current)
               (destructuring-bind (node . edge) (get-val from current)
                 (cons edge (unless (member a edge) (reconstruct-path node))))))
      (setf (get-val g a) 0
            (get-val f a) (funcall heuristic a)
            (get-val open a) t)

      (sb-concurrency:enqueue (get-val f a) fringe)

      (do ((current (sb-concurrency:dequeue fringe) (sb-concurrency:dequeue fringe)))
          ((zerop (hash-table-count open))
           (multiple-value-bind (value present-p) (get-val f b)
             (when present-p
               (values (nreverse (reconstruct-path b)) value))))

        (when (eql current b)
          (return-from shortest-path
            (values (nreverse (reconstruct-path current))
                    (get-val f current))))

        (remhash current open)
        (setf (get-val closed current) t)

        (mapc (lambda (edge)
                (let ((weight (or (edge-value graph edge) 1)))
                  (mapc (lambda (next)
                          (unless (get-val closed next)
                            (setf (get-val open next) t)
                            (let ((tentative (+ (get-val g current) weight)))
                              (multiple-value-bind (value present-p)
                                  (get-val g next)
                                (when (or (not present-p)
                                          (< tentative value))
                                  (setf (get-val from next) (cons current edge)
                                        (get-val g next) tentative
                                        (get-val f next)
                                        (+ tentative (funcall heuristic next)))
                                  (sb-concurrency:enqueue fringe (get-val f next)))))))
                        (etypecase graph
                          (directed-graph (cdr (member current edge)))
                          (graph (remove current edge))))))
              (node-edges graph current))))))

;;; Min Cut
;;
;; Stoer, M. and Wagner, Frank. 1997. A Simple Min-Cut Algorithm.
;; Journal of the ACM
;;
;; Theorem: Let s,t ∈ (nodes G), let G' be the result of merging s and
;;          t in G.  Then (min-cut G) is equal to the minimum of the
;;          min cut of s and t in G and (min-cut G').
(defun weigh-cut (graph cut)
  (reduce #'+ (mapcar {edge-value graph}
                      (remove-if-not (lambda (edge)
                                       (and (intersection edge (first cut))
                                            (intersection edge (second cut))))
                                     (edges graph)))))

(defgeneric min-cut (graph)
  (:documentation
   "Return both the global min-cut of GRAPH and the weight of the cut."))

(defmethod min-cut ((graph graph))
  (let ((g (copy-graph graph))
        (merged-nodes (mapcar (lambda (n) (list n n)) (nodes graph)))
        cuts-of-phase)
    (flet ((connection-weight (group node)
             ;; return the weight of edges between GROUP and NODE
             (reduce #'+ (mapcar {edge-value g}
                                 (remove-if-not {intersection group}
                                                (node-edges g node)))))
           (my-merge (a b)
             ;; merge in the graph
             (merge-nodes g a b)
             ;; update our merged nodes alist
             (setf (cdr (assoc a merged-nodes))
                   (append (cdr (assoc a merged-nodes))
                           (cdr (assoc b merged-nodes))))
             (setq merged-nodes
                   (remove-if (lambda (it) (eql (car it) b)) merged-nodes))))
      (loop while (> (length (nodes g)) 1) do
         (let* ((a (list (random (nodes g))))
                (rest (remove (car a) (nodes g))))
           (loop while rest do
              ;; grow A by adding the node most tightly connected to A
              (let ((new (car (sort rest #'> :key {connection-weight a}))))
                (setf rest (remove new rest))
                (push new a)))
           ;; store the cut-of-phase
           (push (cons (connection-weight (cdr a) (car a))
                       (cdr (assoc (car a) merged-nodes)))
                 cuts-of-phase)
           ;; merge two last added nodes
           (my-merge (first a) (second a))))
      ;; return the minimum cut-of-phase
      (let* ((half (cdar (sort cuts-of-phase #'< :key #'car)))
             (cut  (list half (set-difference (nodes graph) half))))
        (values (sort cut #'< :key #'length) (weigh-cut graph cut))))))

;; https://en.wikipedia.org/wiki/Degeneracy_(graph_theory)

;;; CLOS utils
;; it's often useful to convert a class hierarchy into a GRAPH so that it may
;; easily be printed to DOT (using the DAT/DOT package)

;; TODO 2025-08-19: accept optional direction arg (up = class-direct-superclasses)
(defun class-graph (class)
  "Return a new GRAPH object containing all instances of CLASS."
  (let ((graph (make-instance 'graph)))
    (flet ((.insert (x y)
             (when y
               (mapc
                (lambda (z)
                  (add-edge graph `(,x ,z)))
                y)))
           (.map (x)
             (mapcar
              (lambda (y) (when y (mapcar 'class-name (sb-mop:class-direct-subclasses (find-class y)))))
               x)))
      (let* ((classes (mapcar 'class-name (sb-mop:class-direct-subclasses (find-class class))))
             (subs (.map classes))) ; 2nd level
        (add-node graph class)
        (.insert class classes)
        (loop while subs
              do (loop
                   for c in classes
                   for s in subs
                   do (.insert c s))
              do (setf classes (flatten subs))
              do (setf subs (.map classes)))
      graph))))

;; TODO 2025-08-24: 
(defun g-reader (stream sub num)
  "Parse the next form as a GRAPH object.

#g(:nodes () :edges ()) ;=> #s(graph ...)"
  (declare (ignore sub num))
  (let ((form (print (read stream))))
    (apply 'make-instance 'graph form)))

(defreadtable :graph
  "The graph readtable"
  (:merge :std)
  (:dispatch-macro-char #\# #\g #'g-reader))
