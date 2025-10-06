;;; graph.lisp --- Org Graph Lisp API

;; 

;;; Code:
(in-package :organ/graph)

(load-database-backend :rdb)
(blake3:load-blake3)

;;; Schema
(defclass org-graph-schema (rdb-schema) ()
  (:default-initargs
   :fields (make-fields :file '(pathname . octet-vector)
			:title '(org-id . string)
			:hash '(org-id . string)
			:atime '(org-id . octet-vector)
			:mtime '(org-id . octet-vector)
			:node '(org-id . octet-vector)
			:edge '(org-id . octet-vector)
			:node-tags '(org-id . string)
			:node-links '(org-id . string)
			:node-properties '(org-id . string)
			:node-priority '(org-id . string)
			:node-schedule '(org-id . string)
			:node-file '(org-id . string)
			:node-pos '(org-id . octet-vector)
			:node-state '(org-id . string))))

(defparameter *org-graph-schema* (make-instance 'org-graph-schema))

(defvar *org-graph-file* (merge-pathnames ".emacs.d/graph.sxp" (user-homedir-pathname)))

;;; Org Graph
(defvar *org-graph* nil)
(defvar *org-graph-nodes* nil)
(defvar *org-graph-edges* nil)

(defclass org-graph (directed-graph) ())

(defmethod read-ast ((fmt (eql :org-graph)) stream)
  "Read an ORG-GRAPH specification from STREAM."
  (let* ((ast (read stream))
         (nodes (mapcar 'wrap-node (getf ast :nodes)))
         (graph (make-instance 'org-graph :nodes nodes))
         (edges (mapcar (lambda (x) (add-edge graph (wrap-edge x))) (getf ast :edges))))
    (values graph nodes edges)))

(defmethod build-ast ((self org-graph) &key)
  `(:nodes ,(mapcar 'build-ast (nodes self))
    :edges ,(mapcar 'build-ast (edges self))))

(defun read-org-graph-file (&optional (file *org-graph-file*))
  (with-open-file (f file) 
    (read-ast :org-graph f)))

(defun init-org-graph ()
  (multiple-value-bind (graph nodes edges) (read-org-graph-file)
    (setf *org-graph* graph
          *org-graph-nodes* nodes
          *org-graph-edges* edges)
    graph))

(defclass org-graph-node (vertex) 
  ((name :initarg :name :accessor name) 
   (path :initarg :path :accessor path)
   (point :initarg :point :accessor idx)))

(defclass org-graph-external-node (vertex)
  ((name :initarg :name :accessor name)
   (uri :initarg :uri :accessor uri)))

(defun extract-external-node (edge)
  "Extract an external node from EDGE which should have an EDGE-TYPE eql to
:RELATION. External nodes share the same ID as the edges which they are
extracted from."
  (make-instance 'org-graph-external-node 
    :id (id edge)
    :name (name edge)
    :uri (parse-uri (url:url-encode (edge-out edge)) :escape nil)))

(defun wrap-node (form)
  (make-instance 'org-graph-node 
    :id (make-uuid-from-string (pop form)) 
    :name (pop form) 
    :path (pop form) 
    :point (pop form)))

(defmethod add-node ((graph graph) (node org-graph-node))
  (add-node graph (uuid-to-string (id node))))

(defmethod build-ast ((self org-graph-node) &key)
  `(,(uuid-to-string (id self)) ,(name self) ,(path self) ,(idx self)))

(defclass org-graph-edge (edge) 
  ((type :initarg :type :accessor edge-type)
   (properties :initarg :properties :accessor edge-properties)
   (timestamp :initarg :timestamp :accessor timestamp)
   (point :initarg :point :accessor idx)))

(defmethod name ((self org-graph-edge))
  (when (slot-boundp self 'properties)
    (getf (edge-properties self) :name)))

(defclass org-graph-implicit-edge (org-graph-edge)
  ()
  (:documentation "A graph edge which is created as a result of EXPAND-NODES.")
  (:default-initargs :timestamp (now)))

(defun wrap-edge (form)
  (make-instance 'org-graph-edge
    :type (keywordicate (pop form))
    :in (pop form)
    :properties (pop form)
    :timestamp 
    (destructuring-bind (sec minute hour day month year timezone a1 a2) (pop form)
      (declare (ignore a1 a2))
      (encode-timestamp 0 sec minute hour day month year :timezone (or timezone *default-timezone*)))
    :point (pop form)
    :out (pop form)))

(defmethod build-ast ((self org-graph-edge) &key)
  `(,(keywordicate (edge-type self)) ,(uuid-to-string (edge-in self)) ,(edge-properties self)
    ,(timestamp-to-universal (timestamp self)) ,(idx self) ,(format nil "~A" (edge-out self))))

(defmethod equiv:equiv ((a org-graph-node) (b org-graph-node))
  (uuid= (id a) (id b)))

(defun expand-edges (&optional (edges *org-graph-edges*) (nodes *org-graph-nodes*))
  "Expand a list of EDGES, returning a list of newly discovered nodes."
  (dolist (x edges nodes)
    (when (eql :relation (edge-type x))
      (let ((i (edge-out x)))
        (unless (typep i 'uuid) ;; if it's a uuid it's already a node
          (pushnew
           (extract-external-node x)
           nodes
           :test 'equiv:equiv))))))

(defun expand-nodes (&optional (nodes *org-graph-nodes*) (edges *org-graph-edges*))
  "Expand a list of NODES, returning a list of newly discovered edges."
  (dolist (x nodes edges)))

(defun expand-graph (&optional (graph *org-graph*))
  (setf (edges graph) (expand-nodes (nodes graph))
        (nodes graph) (expand-edges (edges graph)))
  graph)

;;; Files
(defstruct org-graph-file 
  "Internal helper struct used while processing files in the *ORG-GRAPH*."
  path document timestamp hash)

(defun org-graph-extract-files (&optional (graph *org-graph*))
  (let ((ret))
    (std/async::dosequence (n (remove-duplicates (nodes graph) :test 'string= :key 'path) ret)
      (push (wrap (make-org-graph-file) (probe-file (path n))) ret))))

(defmethod id ((self org-graph-file))
  (org-graph-file-hash self))

(defmethod wrap ((self org-graph-file) (file pathname))
  (setf (org-graph-file-hash self) (b3sum file)
	(org-graph-file-path self) file
	(org-graph-file-timestamp self) (universal-to-timestamp (file-write-date file))
	(org-graph-file-document self) (organ:org-parse :document file))
  self)

(defmethod wrap ((self org-graph-file) (node org-graph-node))
  (let ((file (path node)))
    (setf (org-graph-file-hash self) (b3sum file)
	  (org-graph-file-path self) file
	  (org-graph-file-document self) (organ:org-parse :document file))
    self))

;;; Org Graph DB
(defvar *org-graph-db-directory* (merge-pathnames ".store/db/graph/" (user-homedir-pathname)))

(defun make-org-graph-db ()
  (load-schema
   (make-db :rdb :name (namestring *org-graph-db-directory*)
		 :opts (default-rdb-opts))
   *org-graph-schema*))

(defvar *org-graph-db* (make-db :rdb :path *org-graph-db-directory*))

(defun close-org-graph-db ()
  (when (db-open-p *org-graph-db*)
    (shutdown-db *org-graph-db*)))

(defun init-org-graph-db ()
  (ensure-directories-exist
   (make-pathname :directory (butlast (pathname-directory *org-graph-db-directory*)))
   :verbose t)
  (with-db (db :open (not (db-open-p *org-graph-db*)) :close nil :db *org-graph-db*)
    (create-columns db)
    (insert-org-files)
    (insert-org-nodes)
    (insert-org-edges)
    (log:info! "created org-graph-db" *org-graph-db* *org-graph-db-directory* *org-graph-schema*)))

(defun open-org-graph-db ()
  (unless *org-graph* (init-org-graph))
  (unless (probe-file *org-graph-db-directory*)
    (init-org-graph-db))
  (if (and *org-graph-db* (db-open-p *org-graph-db*))
      *org-graph-db*
      (progn
	(rdb:load-opts *org-graph-db*)
	(open-columns* *org-graph-db*))))

(defun destroy-org-graph-db ()
  (unless (db-closed-p *org-graph-db*)
    (close-db *org-graph-db*)
    (log:info! "destroyed org-graph-db at ~A" *org-graph-db-directory*)))

(defun og-get (key &optional (from "node"))
  (get-val *org-graph-db* key :data-type 'string :column from))

(defun og-values (column)
  (with-iter (it (iter *org-graph-db* :column (find-column column *org-graph-db*)))
    seek-to-first
    (loop while iter-valid-p
	  collect (cons (handler-case (octet-vector-to-uuid key)
			  (simple-type-error () (sb-ext:octets-to-string key)))
			(sb-ext:octets-to-string val))
	  do (progn next))))

(defun insert-org-files ()
  (log:info! "inserting org files")
  (mapcar 
   (lambda (n) (insert-key *org-graph-db* (uuid-octets* (id n)) (path n) :column "file"))
   (nodes *org-graph*)))

(defun insert-org-nodes ()
  (log:info! "inserting org nodes")
  (dolist (id (mapcar 'id (nodes *org-graph*)))
    (insert-key *org-graph-db*
                (uuid-octets* id)
                ;; TODO 2024-12-30: 
                #(1)
                :column "node")))

(defun insert-org-edges ()
  (log:info! "inserting org edges")
  (dolist (e (edges *org-graph*))
    (insert-key *org-graph-db* 
                (uuid-octets* (edge-in e)) 
                (uuid-octets* (edge-out e))
                :column "edge")))

;;; Files
(defun org-graph-file-search (path &rest ids)
  "Return a list of org headings corresponding to IDS in PATH. If no IDS are
provided then all are returned."
  ;; first get an org-document and list of headings
  (let* ((doc (organ:org-parse :document path))
	 (headings (organ:doc-tree doc))
	 (ret)
         (ids-p (when ids t)))
    ;; map over IDs, searching for matches
    (loop for h across headings
	  if (typep h 'organ:org-heading)
	  do
	     (when-let* ((prop (organ::org-properties h))
			 (id (find "ID" (organ:org-contents prop)
                                   :key (lambda (x) (string-upcase (name x)))
                                   :test 'equal)))
               (if ids-p
                   (when-let ((found (find (value id) ids :test 'equal)))
		     (removef ids found :test 'equal)
                     (push h ret))
                   (push h ret)))
	  finally (return ret))))

;;; Serde
(defmethod serialize ((self org-graph) format &key stream)
  (serialize (build-ast self) format :stream stream))

(defmethod serialize ((self org-graph) (format (eql :dot)) &key path)
  (dat/dot:graph-to-dot-file self path :attributes '((layout . "sfdp") (beautify . "true"))))


