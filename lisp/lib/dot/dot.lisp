;;; dot.lisp --- GraphViz DOT Language Compiler

;; https://graphviz.org/doc/info/lang.html

;;; Code:
(defpackage :dot
  (:use :cl :std :cl-ppcre)
  (:export
   ;; Variables
   #:*dot-path*
   #:*neato-path*

   ;; Classes
   #:attributed
   #:node
   #:cluster
   #:edge
   #:graph

   ;; Accessors
   #:id-of
   #:attributes-of
   #:nodes-of
   #:edges-of
   #:source-of
   #:target-of

   ;; Graph Protocol
   #:graph-object-knows-of
   #:graph-object-node
   #:graph-object-cluster
   #:graph-object-points-to
   #:graph-object-pointed-to-by
   #:graph-object-edges
   #:generate-graph-from-roots

   ;; Miscellaneous
   #:print-graph
   #:dot-graph

   :find-dot))

(in-package :dot)

;;; Config
(defun find-dot ()
  "Find the DOT program using either the environment variable CL_DOT_DOT, search in the user's
path, or search of likely installation locations."
  (or
   (uiop:getenv "CL_DOT_DOT")
   (check-in-path "dot")
   (loop for file in #+(or win32 mswindows) (list "\"C:/Program Files/ATT/Graphviz/bin/dot.exe\"")
         #-(or win32 mswindows) (list "/usr/local/bin/dot" "/opt/local/bin/dot" "/usr/bin/dot")
         when (probe-file file)
           return file
         finally (return nil))))

(defun find-neato ()
  "Find the NEATO program using either the environment variable CL_DOT_NEATO, search in the user's
path, or search of likely installation locations."
  (or
   (uiop:getenv "CL_DOT_NEATO")
   (check-in-path "neato")
   (loop for file in #+(or win32 mswindows) (list "\"C:/Program Files/ATT/Graphviz/bin/neato.exe\"")
         #-(or win32 mswindows) (list "/usr/local/bin/neato" "/opt/local/bin/neato" "/usr/bin/neato")
         when (probe-file file)
           return file
         finally (return nil))))


(defun check-in-path (name)
  (multiple-value-bind (outstring errstring exit-code)
      (uiop:run-program (list  #+(or win32 mswindows)"where"
                               #-(or win32 mswindows)"which"
                               name) :force-shell t :output '(:string :stripped t) :ignore-error-status t)
    (declare (ignore errstring))
    (when (zerop exit-code) (uiop:parse-native-namestring outstring))))

(defvar *dot-path*
  #+(or win32 mswindows) "\"C:/Program Files/ATT/Graphviz/bin/dot.exe\""
  #-(or win32 mswindows) "/usr/bin/dot"
  "Path to the dot command")

;; the path to the neato executable (used for drawing undirected
;; graphs).
(defvar *neato-path*
  #+(or win32 mswindows) "\"C:/Program Files/ATT/Graphviz/bin/neato.exe\""
  #-(or win32 mswindows) "/usr/bin/neato"
  "Path to the neato command")

(eval-when (:load-toplevel :execute)
  (setf *dot-path* (find-dot))
  (setf *neato-path* (find-neato)))

;;; Dead Code
(defgeneric object-node (object)
  (:documentation
   "Return a NODE instance for this object, or NIL. In the latter case
the object will not be included in the graph, but it can still have an
indirect effect via other protocol functions (e.g. OBJECT-KNOWS-OF).
This function will only be called once for each object during the
generation of a graph."))

(defgeneric object-points-to (object)
  (:documentation
   "Return a list of objects to which the NODE of this object should be
connected. The edges will be directed from this object to the others.
To assign dot attributes to the generated edges, each object can optionally
be wrapped in a instance of ATTRIBUTED.")
  (:method ((object t))
    nil))

(defgeneric object-pointed-to-by (object)
  (:documentation
   "Return a list of objects to which the NODE of this object should be
connected. The edges will be directed from the other objects to this
one. To assign dot attributes to the generated edges, each object can
optionally be wrapped in a instance of ATTRIBUTED.")
  (:method ((object t))
    nil))

(defgeneric object-knows-of (object)
  (:documentation
   "Return a list of objects that this object knows should be part of the
graph, but which it has no direct connections to.")
  (:method ((object t))
    nil))

(defgeneric generate-graph (object &optional attributes)
  (:documentation "Construct a GRAPH with ATTRIBUTES starting
from OBJECT, using the GRAPH-OBJECT- protocol.")
  (:method ((object t) &optional attributes)
    (generate-graph-from-roots 'default (list object) attributes)))

;;; Data
(defparameter *node-shapes*
  '(:record :box :polygon :ellipse :oval :circle :point :egg :triangle
    :plaintext :plain :diamond :trapezium :parallelogram :house :pentagon
    :hexagon :septagon :octagon :doublecircle :doubleoctagon :tripleoctagon
    :invtriangle :invtrapezium :invhouse "Mdiamond" "Msquare" "Mcircle" :rect
    :rectangle :square :star :none :underline :cylinder :note :tab :folder
    :box3d :component :promoter :cds :terminator :utr :primersite
    :restrictionsite :fivepoverhang :threepoverhang :noverhang :assembly
    :signature :insulator :ribosite :rnastab :proteasesite :proteinstab
    :rpromoter :rarrow :larrow :lpromoter))

(defparameter *predefined-arrow-shapes*
  '(:box :lbox :rbox :obox :olbox :orbox :crow :lcrow :rcrow :diamond :ldiamond
    :rdiamond :odiamond :oldiamond :ordiamond :dot :odot :inv :linv :rinv :oinv
    :olinv :orinv :none :normal :lnormal :rnormal :onormal :olnormal :ornormal
    :tee :ltee :rtee :vee :lvee :rvee :curve :lcurve :rcurve :icurve :licurve
    :ricurve))

(defparameter *node-styles*
  '(:solid :dashed :dotted :bold :rounded :diagonals :filled :striped :wedged))

(defparameter *edge-styles* '(:solid :dashed :dotted :bold))

(defparameter *cluster-styles*
  '(:solid :dashed :dotted :bold :rounded :filled :striped))

(defparameter *attributes*
  (list (make-attribute "_background" '(:graph) 'text)
        (make-attribute "area" '(:node :cluster) 'float)
        (make-attribute "arrowhead" '(:edge) *predefined-arrow-shapes*)
        (make-attribute "arrowsize" '(:edge) 'float)
        (make-attribute "arrowtail" '(:edge) *predefined-arrow-shapes*)
        (make-attribute "bb" '(:graph) 'text)
        (make-attribute "bgcolor" '(:graph :cluster) 'text)
        (make-attribute "center" '(:graph) 'boolean)
        (make-attribute "charset" '(:graph) 'text)
        (make-attribute "clusterrank" '(:graph) '(:local :global :none))
        (make-attribute "color" '(:edge :node :cluster) 'text)
        (make-attribute "colorscheme" '(:edge :node :cluster :graph) 'text)
        (make-attribute "comment" '(:edge :node :graph) 'text)
        (make-attribute "compound" '(:graph) 'boolean)
        (make-attribute "concentrate" '(:graph) 'boolean)
        (make-attribute "constraint" '(:edge) 'boolean)
        (make-attribute "Damping" '(:graph) 'float)
        (make-attribute "decorate" '(:edge) 'boolean)
        (make-attribute "defaultdist" '(:graph) 'float)
        (make-attribute "dim" '(:graph) 'integer)
        (make-attribute "dimen" '(:graph) 'integer)
        (make-attribute "dir" '(:edge) '(:forward :back :both :none))
        (make-attribute "diredgeconstraints" '(:graph) 'text)
        (make-attribute "distortion" '(:node) 'float)
        (make-attribute "dpi" '(:graph) 'float)
        (make-attribute "edgehref" '(:edge) 'text)
        (make-attribute "edgetarget" '(:edge) 'text)
        (make-attribute "edgetooltip" '(:edge) 'text)
        (make-attribute "edgeURL" '(:edge) 'text)
        (make-attribute "epsilon" '(:graph) 'float)
        (make-attribute "esep" '(:graph) 'text)
        (make-attribute "fillcolor" '(:node :edge :cluster) 'text)
        (make-attribute "fixedsize" '(:node) 'text)
        (make-attribute "fontcolor" '(:edge :node :graph :cluster) 'text)
        (make-attribute "fontname" '(:edge :node :graph :cluster) 'text)
        (make-attribute "fontnames" '(:graph) 'text)
        (make-attribute "fontpath" '(:graph) 'text)
        (make-attribute "fontsize" '(:edge :node :graph :cluster) 'float)
        (make-attribute "forcelabels" '(:graph) 'boolean)
        (make-attribute "gradientangle" '(:node :cluster :graph) 'integer)
        (make-attribute "group" '(:node) 'text)
        (make-attribute "head_lp" '(:edge) 'text)
        (make-attribute "headclip" '(:edge) 'boolean)
        (make-attribute "headhref" '(:edge) 'text)
        (make-attribute "headlabel" '(:edge) 'label-text)
        (make-attribute "headport" '(:edge) 'text)
        (make-attribute "headtarget" '(:edge) 'text)
        (make-attribute "headtooltip" '(:edge) 'text)
        (make-attribute "headURL" '(:edge) 'text)
        (make-attribute "height" '(:node) 'float)
        (make-attribute "href" '(:graph :cluster :node :edge) 'text)
        (make-attribute "id" '(:graph :cluster :node :edge) 'text)
        (make-attribute "image" '(:node) 'text)
        (make-attribute "imagepath" '(:graph) 'text)
        (make-attribute "imagescale" '(:node) 'text)
        (make-attribute "inputscale" '(:graph) 'float)
        (make-attribute "K" '(:graph :cluster) 'float)
        (make-attribute "label" '(:edge :node :graph :cluster) 'label-text)
        (make-attribute "label_scheme" '(:graph) 'integer)
        (make-attribute "labelangle" '(:edge) 'float)
        (make-attribute "labeldistance" '(:edge) 'float)
        (make-attribute "labelfloat" '(:edge) 'boolean)
        (make-attribute "labelfontcolor" '(:edge) 'text)
        (make-attribute "labelfontname" '(:edge) 'text)
        (make-attribute "labelfontsize" '(:edge) 'float)
        (make-attribute "labelhref" '(:edge) 'text)
        (make-attribute "labeljust" '(:graph :cluster) 'text)
        (make-attribute "labelloc" '(:node :graph :cluster) 'text)
        (make-attribute "labeltarget" '(:edge) 'text)
        (make-attribute "labeltooltip" '(:edge) 'text)
        (make-attribute "labelURL" '(:edge) 'text)
        (make-attribute "landscape" '(:graph) 'boolean)
        (make-attribute "layer" '(:edge :node :cluster) 'text)
        (make-attribute "layerlistsep" '(:graph) 'text)
        (make-attribute "layers" '(:graph) 'text)
        (make-attribute "layerselect" '(:graph) 'text)
        (make-attribute "layersep" '(:graph) 'text)
        (make-attribute "layout" '(:graph) 'text)
        (make-attribute "len" '(:edge) 'float)
        (make-attribute "levels" '(:graph) 'integer)
        (make-attribute "levelsgap" '(:graph) 'float)
        (make-attribute "lhead" '(:edge) 'text)
        (make-attribute "lheight" '(:graph :cluster) 'float)
        (make-attribute "lp" '(:edge :graph :cluster) 'text)
        (make-attribute "ltail" '(:edge) 'text)
        (make-attribute "lwidth" '(:graph :cluster) 'float)
        (make-attribute "margin" '(:node :cluster :graph) 'text)
        (make-attribute "maxiter" '(:graph) 'integer)
        (make-attribute "mclimit" '(:graph) 'float)
        (make-attribute "mindist" '(:graph) 'float)
        (make-attribute "minlen" '(:edge) 'integer)
        (make-attribute "mode" '(:graph) 'text)
        (make-attribute "model" '(:graph) 'text)
        (make-attribute "mosek" '(:graph) 'boolean)
        (make-attribute "newrank" '(:graph) 'boolean)
        (make-attribute "nodesep" '(:graph) 'float)
        (make-attribute "nojustify" '(:graph :cluster :node :edge) 'boolean)
        (make-attribute "normalize" '(:graph) 'text)
        (make-attribute "notranslate" '(:graph) 'boolean)
        (make-attribute "nslimit" '(:graph) 'float)
        (make-attribute "ordering" '(:graph :node) 'text)
        (make-attribute "orientation" '(:node) 'float)
        (make-attribute "orientation" '(:graph) 'text)
        (make-attribute "outputorder" '(:graph)
                        '(:breadthfirst :nodesfirst :edgesfirst))
        (make-attribute "overlap" '(:graph) 'text)
        (make-attribute "overlap_scaling" '(:graph) 'float)
        (make-attribute "overlap_shrink" '(:graph) 'boolean)
        (make-attribute "pack" '(:graph) 'text)
        (make-attribute "packmode" '(:graph) 'text)
        (make-attribute "pad" '(:graph) 'text)
        (make-attribute "page" '(:graph) 'text)
        (make-attribute "pagedir" '(:graph)
                        '("BL" "BR" "TL" "TR" "RB" "RT" "LB" "LT"))
        (make-attribute "pencolor" '(:cluster) 'text)
        (make-attribute "penwidth" '(:cluster :node :edge) 'float)
        (make-attribute "peripheries" '(:node :cluster) 'integer)
        (make-attribute "pin" '(:node) 'boolean)
        (make-attribute "pos" '(:edge :node) 'text)
        (make-attribute "quadtree" '(:graph) 'text)
        (make-attribute "quantum" '(:graph) 'float)
        (make-attribute "rank" '(:subgraph) '(:same :min :source :max :sink))
        (make-attribute "rankdir" '(:graph) '("TB" "LR" "BT" "RL"))
        (make-attribute "ranksep" '(:graph) 'text)
        (make-attribute "ratio" '(:graph) 'text)
        (make-attribute "rects" '(:node) 'text)
        (make-attribute "regular" '(:node) 'boolean)
        (make-attribute "remincross" '(:graph) 'boolean)
        (make-attribute "repulsiveforce" '(:graph) 'float)
        (make-attribute "resolution" '(:graph) 'float)
        (make-attribute "root" '(:graph :node) 'text)
        (make-attribute "rotate" '(:graph) 'integer)
        (make-attribute "rotation" '(:graph) 'float)
        (make-attribute "samehead" '(:edge) 'text)
        (make-attribute "sametail" '(:edge) 'text)
        (make-attribute "samplepoints" '(:node) 'integer)
        (make-attribute "scale" '(:graph) 'text)
        (make-attribute "searchsize" '(:graph) 'integer)
        (make-attribute "sep" '(:graph) 'text)
        (make-attribute "shape" '(:node) *node-shapes*)
        (make-attribute "shapefile" '(:node) 'text)
        (make-attribute "showboxes" '(:edge :node :graph) 'integer)
        (make-attribute "sides" '(:node) 'integer)
        (make-attribute "size" '(:graph) 'text)
        (make-attribute "skew" '(:node) 'float)
        (make-attribute "smoothing" '(:graph)
                        '(:none :avg_dist :graph_dist :power_dist :rng :spring
                          :triangle))
        (make-attribute "sortv" '(:graph :cluster :node) 'integer)
        (make-attribute "splines" '(:graph) 'text)
        (make-attribute "start" '(:graph) 'text)
        (make-attribute "style" '(:node) *node-styles*)
        (make-attribute "style" '(:edge) *edge-styles*)
        (make-attribute "style" '(:cluster) *cluster-styles*)
        (make-attribute "stylesheet" '(:graph) 'text)
        (make-attribute "tail_lp" '(:edge) 'text)
        (make-attribute "tailclip" '(:edge) 'boolean)
        (make-attribute "tailhref" '(:edge) 'text)
        (make-attribute "taillabel" '(:edge) 'label-text)
        (make-attribute "tailport" '(:edge) 'text)
        (make-attribute "tailtarget" '(:edge) 'text)
        (make-attribute "tailtooltip" '(:edge) 'text)
        (make-attribute "tailURL" '(:edge) 'text)
        (make-attribute "target" '(:edge :node :graph :cluster) 'text)
        (make-attribute "tooltip" '(:node :edge :cluster) 'text)
        (make-attribute "truecolor" '(:graph) 'boolean)
        (make-attribute "URL" '(:edge :node :graph :cluster) 'text)
        (make-attribute "vertices" '(:node) 'text)
        (make-attribute "viewport" '(:graph) 'text)
        (make-attribute "voro_margin" '(:graph) 'float)
        (make-attribute "weight" '(:edge) 'text)
        (make-attribute "width" '(:node) 'float)
        (make-attribute "xdotversion" '(:graph) 'text)
        (make-attribute "xlabel" '(:edge :node) 'label-text)
        (make-attribute "xlp" '(:node :edge) 'text)
        (make-attribute "z" '(:node) 'float)))

;;; Context 
(deftype context ()
  "A context in which an attribute may occur."
  '(member :graph :subgraph :cluster :node :edge))

(defun context-list-p (thing)
  (and (listp thing)
       (every (lambda (element) (typep element 'context)) thing)))

(deftype context-set ()
  "A set of contexts in which an attribute may occur."
  '(satisfies context-list-p))

(defun foreign-name->lisp-name (name)
  "Return an idiomatic Lisp name derived from the GraphViz name NAME."
  (intern (string-upcase (substitute #\- #\_ name)) :keyword))

(defstruct (attribute
             (:constructor make-attribute (foreign-name allowed-in type
                                           &aux
                                           (name (foreign-name->lisp-name
                                                  foreign-name))))
             (:predicate nil)
             (:copier nil))
  "Description of a GraphViz attribute."
  (name         nil :type symbol           :read-only t)
  (foreign-name nil :type string           :read-only t)
  (allowed-in   nil :type context-set      :read-only t)
  (type         nil :type (or symbol cons) :read-only t))

(defun find-attribute (name attributes)
  (or (find name attributes :key #'attribute-name)
      (error "Invalid attribute ~S" name)))

(defparameter *graph-attributes*
  (remove :graph *attributes* :test-not #'member :key #'attribute-allowed-in))

(defparameter *node-attributes*
  (remove :node *attributes* :test-not #'member :key #'attribute-allowed-in))

(defparameter *edge-attributes*
  (remove :edge *attributes* :test-not #'member :key #'attribute-allowed-in))

(defparameter *cluster-attributes*
  (remove :cluster *attributes* :test-not #'member :key #'attribute-allowed-in))

;;; Classes

(defvar *id*)

(defclass id-mixin ()
  ((id :initform (incf *id*) :initarg :id :accessor id-of)))

(defclass attributes-mixin ()
  ((attributes :initform nil :initarg :attributes :accessor attributes-of)))

(defclass graph (attributes-mixin)
  ((nodes :initform nil :initarg :nodes :accessor nodes-of)
   (edges :initform nil :initarg :edges :accessor edges-of)
   ;; A hash table, mapping from clusters to lists of nodes.  The hash
   ;; table also contains one entry whose key is NIL, and whose value is
   ;; the list of nodes that are not part of a cluster.
   (cluster-nodes
    :initform (make-hash-table)
    :initarg :cluster-nodes
    :accessor cluster-nodes-of)))

(defclass node (id-mixin
                attributes-mixin)
  ()
  (:documentation "A graph node with `dot` attributes (a plist, initarg
:ATTRIBUTES) and an optional `dot` id (initarg :ID, autogenerated
by default)."))

(defclass port-mixin ()
  ((source-port :initform nil :initarg :source-port :accessor source-port-of)
   (target-port :initform nil :initarg :target-port :accessor target-port-of)))

(defclass attributed (attributes-mixin
                      port-mixin)
  ((object :initarg :object :accessor object-of))
  (:documentation "Wraps an object (initarg :OBJECT) with `dot` attribute
information (a plist, initarg :ATTRIBUTES)"))

(defmethod print-object ((object attributed) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (object-of object))))

(defclass edge (attributes-mixin
                port-mixin)
  ((source :initform nil :initarg :source :accessor source-of)
   (target :initform nil :initarg :target :accessor target-of)))

(defclass cluster (id-mixin
                   attributes-mixin)
  ()
  (:documentation "A cluster with `dot` attributes (a plist, initarg
:ATTRIBUTES) and an optional `dot` id (initarg :ID, autogenerated
by default)."))

;;; Protocol functions

(defgeneric graph-object-node (graph object)
  (:documentation
   "Returns a NODE instance for this object, or NIL.

In the latter case the object will not be included in the graph, but
it can still have an indirect effect via other protocol
functions (e.g. GRAPH-OBJECT-KNOWS-OF).  This function will only be
called once for each object during the generation of a graph.")
  (:method ((graph (eql 'default)) object)
    (declare (ignore graph))
    (object-node object)))

(defgeneric graph-object-cluster (graph object)
  (:documentation
   "Returns a CLUSTER instance for this object, or NIL.

The nodes nodes of objects for which this function returns the same cluster
are grouped together as a subgraph.  This function will only be called once
for each object during the generation of a graph.")
  (:method (graph object)
    (declare (ignore graph object))
    nil))

(defgeneric graph-object-edges (graph)
  (:documentation
   "Returns a sequence of edge specifications.

An edge specification is a list (FROM TO [ATTRIBUTES]), where FROM and
TO are objects of the graph and optional ATTRIBUTES is a plist of edge
attributes.")
  (:method (graph)
    (declare (ignore graph))
    '()))

(defgeneric graph-object-points-to (graph object)
  (:documentation
   "Returns a sequence of objects to which the NODE of this object
should be connected.

The edges will be directed from this object to the others.  To assign
dot attributes to the generated edges, each object can optionally be
wrapped in a instance of ATTRIBUTED.")
  (:method ((graph (eql 'default)) object)
    (declare (ignore graph))
    (object-points-to object))
  (:method (graph (object t))
    '()))

(defgeneric graph-object-pointed-to-by (graph object)
  (:documentation
   "Returns a sequence of objects to which the NODE of this object
should be connected.

The edges will be directed from the other objects to this one. To
assign dot attributes to the generated edges, each object can
optionally be wrapped in a instance of ATTRIBUTED.")
  (:method ((graph (eql 'default)) object)
    (declare (ignore graph))
    (object-pointed-to-by object))
  (:method (graph (object t))
    '()))

(defgeneric graph-object-knows-of (graph object)
  (:documentation
   "Returns a sequence of objects that this object knows should be
part of the graph, but which it has no direct connections to.")
  (:method ((graph (eql 'default)) object)
    (declare (ignore graph))
    (object-knows-of object))
  (:method (graph (object t))
    '()))

;;; Public interface

(defgeneric generate-graph-from-roots (graph objects &optional attributes)
  (:documentation "Constructs a GRAPH with ATTRIBUTES starting
from OBJECTS, using the GRAPH-OBJECT- protocol.")
  (:method (graph objects &optional attributes)
    (multiple-value-bind (nodes edges cluster-nodes)
        (construct-graph graph objects)
      (make-instance 'graph
                     :attributes attributes
                     :nodes nodes
                     :edges edges
                     :cluster-nodes cluster-nodes))))

(defun print-graph (graph &rest options
                    &key (stream *standard-output*) (directed t))
  "Prints a dot-format representation GRAPH to STREAM."
  (declare (ignore stream directed))
  (apply #'generate-dot
         (cluster-nodes-of graph)
         (edges-of graph)
         (attributes-of graph)
         options))

(defun dot-graph (graph outfile &key (format :ps) (directed t))
  "Renders GRAPH to OUTFILE by running the program in \*DOT-PATH* or
*NEATO-PATH* depending on the value of the DIRECTED keyword
argument.  The default is a directed graph.  The default
FORMAT is Postscript."
  (when (null format) (setf format :ps))

  (let ((dot-path (if directed *dot-path* *neato-path*))
        (format (format nil "-T~(~a~)" format))
        (dot-string (with-output-to-string (stream)
                      (print-graph graph
                                   :stream stream
                                   :directed directed))))
    (unless dot-path
      (error "neither 'dot' or 'neato' binary are found.
Consider something like sudo apt install graphviz!"))
    (uiop:run-program (list dot-path format "-o" (namestring outfile))
                      :input (make-string-input-stream dot-string)
                      :output *standard-output*)))

;;; Internal
(defun construct-graph (graph objects)
  (let ((handled-objects (make-hash-table))
        (nodes '())
        (edges '())
        (cluster-nodes (make-hash-table))
        (*id* 0))
    (labels ((add-edge (source target attributes &optional source-port target-port)
               (let ((edge (make-instance 'edge
                                          :attributes attributes
                                          :source source
                                          :source-port source-port
                                          :target target
                                          :target-port target-port)))
                 (push edge edges)))
             (get-node (object)
               (if (typep object 'attributed)
                   (multiple-value-call #'values
                     (get-node (object-of object))
                     (source-port-of object)
                     (target-port-of object))
                   (gethash object handled-objects)))
             (get-attributes (object)
               (when (typep object 'attributed)
                 (attributes-of object)))
             (handle-object (object)
               (when (typep object 'attributed)
                 (return-from handle-object
                   (handle-object (object-of object))))
               ;; If object has been already been visited, skip
               (unless (nth-value 1 (get-node object))
                 (let ((node (graph-object-node graph object))
                       (cluster (graph-object-cluster graph object))
                       (knows-of (graph-object-knows-of graph object))
                       (points-to (graph-object-points-to graph object))
                       (pointed-to (graph-object-pointed-to-by graph object)))
                   (setf (gethash object handled-objects) node)
                   (map nil #'handle-object knows-of)
                   (map nil #'handle-object points-to)
                   (map nil #'handle-object pointed-to)
                   (when node
                     (push node (gethash cluster cluster-nodes '()))
                     (push node nodes)
                     (map nil
                          (lambda (to)
                            (multiple-value-bind (target found? source-port target-port)
                                (get-node to)
                              (when found?
                                (add-edge node target (get-attributes to)
                                          source-port target-port))))
                          points-to)
                     (map nil
                          (lambda (from)
                            (multiple-value-bind (source found? source-port target-port)
                                (get-node from)
                              (when found?
                                (add-edge source node (get-attributes from)
                                          source-port target-port))))
                          pointed-to)))))
             (handle-edge (from to &optional attributes)
               (handle-object from)
               (handle-object to)
               (let ((source (get-node from))
                     (target (get-node to)))
                 (add-edge source target attributes))))
      (map nil #'handle-object objects)
      (map nil
           (lambda (edge-spec)
             (apply #'handle-edge edge-spec))
           (graph-object-edges graph))
      (values nodes edges cluster-nodes))))

(defun generate-dot (cluster-nodes edges attributes
                     &key (stream *standard-output*) (directed t))
  (with-standard-io-syntax ()
    (let ((*standard-output* (or stream *standard-output*))
          (*print-right-margin* 65535)
          (edge-op (if directed "->" "--"))
          (graph-type (if directed "digraph" "graph"))
          (node-defaults '())
          (edge-defaults '()))
      (format stream "~a {~%" graph-type)
      (loop for (name value) on attributes by #'cddr do
           (case name
             (:node
              (setf node-defaults (append node-defaults value)))
             (:edge
              (setf edge-defaults (append edge-defaults value)))
             (t
              (print-key-value stream name value *graph-attributes*)
              (format stream ";~%"))))
      ;; Default attributes.
      (print-defaults stream "node" node-defaults *node-attributes*)
      (print-defaults stream "edge" edge-defaults *edge-attributes*)
      ;; Clusters of nodes.
      (maphash
       (lambda (cluster nodes)
         (if (null cluster)
             (dolist (node nodes)
               (format stream "  ~a " (textify (id-of node)))
               (print-attributes stream (attributes-of node) *node-attributes*)
               (format stream ";~%"))
             (progn
               (format stream "  subgraph cluster_~d {~%" (id-of cluster))
               (loop for (name value) on (attributes-of cluster) by #'cddr do
                 (format stream "  ")
                 (print-key-value stream name value *cluster-attributes*)
                 (format stream ";~%"))
               (dolist (node nodes)
                 (format stream "    ~a " (textify (id-of node)))
                 (print-attributes stream (attributes-of node) *node-attributes*)
                 (format stream ";~%"))
               (format stream "  }~%"))))
       cluster-nodes)
      ;; Edges.
      (dolist (edge edges)
        (format stream "  ~a~@[:~a~] ~a ~a~@[:~a~]"
                (textify (id-of (source-of edge))) (source-port-of edge)
                edge-op
                (textify (id-of (target-of edge))) (target-port-of edge))
        (print-attributes stream (attributes-of edge) *edge-attributes*)
        (format stream ";~%"))
      (format stream "}")
      (values))))

(defun print-defaults (stream kind attributes schema)
  (when attributes
    (format stream "  ~A " kind)
    (print-attributes stream attributes schema)
    (format stream "~%")))

(defun print-attributes (stream attributes schema)
  (format stream "[")
  (loop for (name value) on attributes by #'cddr
     for prefix = "" then "," do
       (write-string prefix)
       (print-key-value stream name value schema))
  (format stream "]"))

(defun print-key-value (stream key value attributes)
  (let* ((attribute    (find-attribute key attributes))
         (foreign-name (attribute-foreign-name attribute))
         (type         (attribute-type attribute)))
    (flet ((text-value (value)
             (typecase value
               (cons
                (destructuring-bind (alignment value) value
                  (textify value :alignment alignment)))
               (t
                (textify value)))))
      (format stream "~a=~a" foreign-name
              (etypecase type
                ((member integer)
                 (unless (typep value 'integer)
                   (error "Invalid value for ~S: ~S is not an integer"
                          key value))
                 value)
                ((member boolean)
                 (if value
                     "true"
                     "false"))
                ((member label-text)
                 (typecase value
                   ((cons (eql :html))
                    (htmlify value))
                   (t
                    (text-value value))))
                ((member text)
                 (text-value value))
                ((member float)
                 (coerce value 'single-float))
                (list
                 (flet ((stringify (value)
                          (unless (member value type :test 'equal)
                            (error "Invalid value for ~S: ~S is not one of ~S"
                                   key value type))
                          (if (symbolp value)
                              (string-downcase value)
                              value)))
                   (if (listp value)
                       (format nil "\"~{~A~^,~}\"" (mapcar #'stringify value))
                       (stringify value)))))))))

(defun htmlify (object)
  (check-type object (cons (eql :html) (cons null)))
  (with-output-to-string (stream)
    (labels
        ((escape-string (string &optional (stream stream))
           (loop :for c :across string :do
              (case c
                (#\"
                 (write-string "&quot;" stream))
                (#\<
                 (write-string "&lt;" stream))
                (#\>
                 (write-string "&gt;" stream))
                (#\&
                 (write-string "&amp;" stream))
                (#\Newline
                 (write-string "<br/>" stream))
                (t
                 (write-char c stream)))))
         (escape-attribute (attribute)
           (list
            (first attribute)
            (with-output-to-string (stream)
              (escape-string (second attribute) stream))))
         (textify-node (node)
           (etypecase node
             (cons
              (destructuring-bind (name attributes &rest children) node
                (format stream "<~A~@[ ~{~{~A=\"~A\"~}~^ ~}~]>"
                        name (mapcar #'escape-attribute attributes))
                (mapc #'textify-node children)
                (format stream "</~A>" name)))
             (string
              (escape-string node)))))
      (write-char #\< stream)
      (mapc #'textify-node (nthcdr 2 object))
      (write-char #\> stream))))

(defun textify (object &key alignment)
  (check-type alignment (member nil :center :left :right))
  (let ((string (princ-to-string object))
        (alignment (or alignment :center)))
    (with-output-to-string (stream)
      (write-char #\" stream)
      (loop for c across string do
            ;; Note: #\\ should not be escaped to allow \n, \l, \N, etc.
            ;; to work.
            (case c
              ((#\")
               (write-char #\\ stream)
               (write-char c stream))
              (#\Newline
               (write-char #\\ stream)
               (ecase alignment
                 (:center
                  (write-char #\n stream))
                 (:left
                  (write-char #\l stream))
                 (:right
                  (write-char #\r stream))))
              (t
               (write-char c stream))))
      (write-char #\" stream))))
