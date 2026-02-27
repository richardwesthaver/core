;;; dot.lisp --- Graphviz DOT format

;;

;;; Commentary:

;; adapted from eschulte's graph library

;; ref: https://github.com/eschulte/graph/blob/master/dot.lisp

;;; Code:
(in-package :dat/dot)

(in-readtable :std) ;; uses curry macros

;;; Visualization
(defstruct rank
  "The information needed to specify a DOT rank statement. VALUE
  expects a string and NODE-LIST expects a list."
  value
  node-list)

(defun rank-print (r)
  "Returns a string containing a DOT rank statement. R is a RANK structure."
  (when (rank-p r))
  (with-output-to-string (out)
    (when (and (rank-value r) (rank-node-list r))
      (format out "{rank=~a;" (rank-value r))
      (mapc (lambda (n)
              (format out " ~s;" n))
            (rank-node-list r))
      (format out " }~%"))))

(defstruct subgraph
  "The information needed to specify a DOT subgraph. NODE-ATTRIBUTES,
EDGE-ATTRIBUTES, and ATTRIBUTES expect assoc lists, and NODE-LIST
expects a list."
  node-attributes
  edge-attributes
  attributes
  ranks
  node-list)

(defun subgraph-print (s)
  "Returns a string containing a DOT subgraph statement. S is a
SUBGRAPH structure."
  (when (subgraph-p s)
    (with-output-to-string (out)
      (format out "subgraph ~a {~%" (string (gensym "cluster_")))
      (when (subgraph-node-attributes s)
        (format out "  node [~a];~%"
                (mapc (lambda (pair)
                        (format out "~a=~a, " (car pair) (cdr pair)))
                      (subgraph-node-attributes s))))
      (when (subgraph-edge-attributes s)
        (format out "  edge [~a];~%"
                (mapc (lambda (pair)
                        (format out "~a=~a, " (car pair) (cdr pair)))
                      (subgraph-edge-attributes s))))
      (when (subgraph-attributes s)
        (mapc (lambda (pair)
                (format out "  ~a=\"~a\";~%" (car pair) (cdr pair)))
              (subgraph-attributes s)))
      (when (subgraph-ranks s)
        (mapcar #'rank-print (subgraph-ranks s)))
      (when (subgraph-node-list s)
        (mapc (lambda (n)
                (format out "  ~a;~%" n))
              (subgraph-node-list s)))
      (format out "  }~%"))))

(defun edge-to-dot (edge graph attrs &optional stream)
  (format stream "  \"~a\" ~a \"~a\" ~{~a~^ ~};~%"
          (edge-in edge)
          (etypecase graph
            (directed-graph "->")
            (graph "--"))
          (edge-out edge)
          (mapcar (lambda (l)
                    (destructuring-bind (attr . fn) l
                      (let ((val (funcall fn edge)))
                        (if val
                            (if (search "URL" (string attr))
                                (format nil "[~a=~a]"
                                        (string-downcase
                                         (string attr)
                                         :end (- (length (string attr)) 3))
                                        val)
                                (format nil "[~(~a~)=~a]" attr val)) ""))))
                    attrs)))

(defun node-to-dot (node attrs &optional stream)
  (format stream "  \"~a\" ~{~a~^ ~};~%" node
          (mapcar (lambda (l) (destructuring-bind (attr . fn) l
                               (let ((val (funcall fn node)))
                                 (if val (if (search "URL" (string attr))
                                             (format nil "[~a=~a]" attr val)
                                             (format nil "[~(~a~)=~a]" attr val)) ""))))
                  attrs)))

(defun graph-to-dot (graph
                     &key stream attributes node-attrs (edge-attrs `(,(cons :label (lambda (x) (format nil "\"~A\"" (name x))))))
                          subgraphs ranks)
  "Print the dot code representing GRAPH. The keyword
argument ATTRIBUTES takes an assoc list with DOT graph attribute (name
. value) pairs. NODE-ATTRS and EDGE-ATTRS also take assoc lists of DOT graph
attributes and functions taking nodes or edges respectively and returning
values. The DOT graph, node, and edge attributes are described at
http://www.graphviz.org/doc/info/attrs.html. SUBGRAPHS is a list of SUBGRAPH
structures.  RANKS is a list of RANK structures."
  ;; by default edges are labeled with their values
  (declare (graph graph))
  (format stream "~a to_dot {~%~{~a~}}~%"
          (etypecase graph
            (directed-graph "digraph")
            (graph "graph"))
          (append
           (mapcar (lambda (l)
                     (destructuring-bind (a . b) l
                       (if (search "URL" (string a))
                           (format nil "  ~a=~a;~%" a b)
                           (format nil "  ~(~a~)=~a;~%" a b))))
                   attributes)
           (mapcar {node-to-dot _ node-attrs}
                   (let ((n (nodes graph)))
                     (etypecase n
                       (hash-table (hash-table-keys n))
                       (sequence (map 'list (lambda (x) 
                                              (string-downcase 
                                               (if (typep (id:id x) 'uuid:uuid)
                                                   (uuid:uuid-to-string (id:id x))
                                                   (id:id x))))
                                      n)))))
           (mapcar {edge-to-dot _ graph edge-attrs}
                   (let ((e (edges graph)))
                     (if (hash-table-p e)
                         ;; FIX 2026-02-26: 
                         (hash-table-keys e)
                         e)))
           (mapcar #'subgraph-print subgraphs)
           (mapcar #'rank-print ranks)))
  (values))

(defun graph-to-dot-file (graph path 
                              &key attributes node-attrs edge-attrs
                                   subgraphs ranks)
  "Write a dot representation of GRAPH to PATH."
  (with-open-file (out path :direction :output :if-exists :supersede)
    (graph-to-dot graph :stream out :attributes attributes :node-attrs node-attrs
                   :edge-attrs edge-attrs :subgraphs subgraphs :ranks ranks)))

(defun graph-from-dot (dot-string)
  "Parse the DOT format string DOT-STRING into a graph.
More robust behavior may be achieved through parsing the output of the
dot executable."
  (flet ((string->symbol (string) (intern (string-upcase string))))
    (let* ((graph-type-re "^ *((di)?graph)")
           (spec-re       "[\\s]*(\\[([^]]+)\\])?;")
           (node-name-re  "[\\s]*\"?([a-zA-Z0-9_]+)\"?")
           (node-spec-re  (concatenate 'string node-name-re spec-re))
           (edge-spec-re  (concatenate 'string
                                       node-name-re "[\\s]+([->]+)" node-name-re spec-re))
           (label-name-re "label=(\"([^\"]+)\"|([^, ]+))[,\\]]")
           (number-re     "[0-9.\/e]+")
           (graph (multiple-value-bind (string matches)
                      (ppcre:scan-to-strings graph-type-re dot-string)
                    (declare (ignorable string))
                    (make-instance (string->symbol (aref matches 0))))))
      ;; add nodes
      (ppcre:do-register-groups (node spec) (node-spec-re dot-string)
        (declare (ignorable spec))
        (unless (member node '("node" "graph") :test 'string=)
          (add-node graph (symbolicate node))))
      ;; add edges
      (ppcre:do-register-groups (left arrow right spec) (edge-spec-re dot-string)
        (declare (ignorable arrow))
        (multiple-value-bind (matchp regs) (ppcre:scan-to-strings label-name-re spec)
          (add-edge graph
                    (mapcar #'symbolicate (list left right))
                    (when matchp
                      (if (ppcre:scan number-re (aref regs 1))
                          (read-from-string (aref regs 1)))))))
      graph)))

;;; Serde
(defmethod serialize ((self graph) (fmt (eql :dot))
                      &key stream path attributes node-attrs edge-attrs
                           subgraphs ranks)
  (declare (ignore fmt))
  (cond
    ((and stream path) (error "passed both STREAM and PATH - pick one"))
    (stream (graph-to-dot self :stream stream :attributes attributes :node-attrs node-attrs
                               :edge-attrs edge-attrs :subgraphs subgraphs :ranks ranks))
    (path (graph-to-dot-file self path :attributes attributes :node-attrs node-attrs
                             :edge-attrs edge-attrs :subgraphs subgraphs :ranks ranks))))

(defmethod deserialize ((from string) (fmt (eql :dot)) &key)
  (declare (ignore fmt))
  (graph-from-dot from))

;; (defun write-dot-stream (object stream)
;;   "Write OBJECT to STREAM in Graphviz DOT format.")

;; (defun write-dot-file (path object)
;;   "Write OBJECT to file PATH in Graphviz DOT format.")

;; (defun read-dot-stream (stream)
;;   "Read from STREAM in Graphviz DOT format.")

;; (defun read-dot-file (path)
;;   "Read from file PATH in Graphviz DOT format.")
