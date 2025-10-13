;;; graph.lisp --- Org Graph Lisp API

;; 

;;; Code:
(in-package :organ/graph)

(load-database-backend :rdb)

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
    (setf *org-graph* graph)
    (setf *org-graph-nodes* (sort (copy-list nodes) #'string< :key (lambda (x) (namestring (path x))))
          *org-graph-edges* edges)
    (setf *org-graph-files* (org-graph-extract-files))
    (setf *org-graph-headings* (expand-nodes))
    (setf *org-graph-edges* (expand-headings))
    graph))

(defun org-graph-stats ()
  `(:node-count ,(length *org-graph-nodes*)
    :edge-count ,(length *org-graph-edges*)
    :file-count ,(length *org-graph-files*)))

(defclass org-graph-node (vertex) 
  ((name :initarg :name :accessor name) 
   (path :initarg :path :accessor path)
   (point :initarg :point :accessor idx)
   (properties :initarg :properties :accessor node-properties)))

(defclass org-graph-external-node (org-graph-node)
  ((name :initarg :name :accessor name)))

(defaccessor uri ((self org-graph-external-node)) (path self))

(defun extract-external-node (edge)
  "Extract an external node from EDGE which should have an EDGE-TYPE eql to
:RELATION. External nodes share the same NAME as the edges which they are
extracted from and share the inherits an ID from the OUT slot."
  (make-instance 'org-graph-external-node 
    :id (edge-out edge)
    :name (name edge)
    :path (or (ignore-errors (parse-uri (edge-out edge))) (url-decode (edge-out edge) :lenient t))
    :point (idx edge)))

(defun wrap-node (form)
  (make-instance 'org-graph-node 
    :id (make-uuid-from-string (pop form)) 
    :name (pop form) 
    :path (pop form) 
    :point (pop form)
    :properties 
    (let ((props (pop form)))
      (when-let ((c (getf props :created)))
        (setf (getf props :created) (org-parse-time c)))
      props)))

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

(defun org-parse-time (s)
  (destructuring-bind (sec minute hour day month year timezone a1 a2) s
    (declare (ignore a1 a2))
    (encode-timestamp 0 sec minute hour day month year :timezone (or timezone *default-timezone*))))

(defun wrap-edge (form)
  (make-instance 'org-graph-edge
    :type (keywordicate (pop form))
    :in (pop form)
    :out (pop form)
    :timestamp (org-parse-time (pop form))
    :point (pop form)
    :properties (pop form)))

(defmethod build-ast ((self org-graph-edge) &key)
  `(,(keywordicate (edge-type self)) ,(uuid-to-string (edge-in self)) ,(edge-properties self)
    ,(timestamp-to-universal (timestamp self)) ,(idx self) ,(format nil "~A" (edge-out self))))

(defmethod equiv:equiv ((a org-graph-node) (b org-graph-node))
  (uuid= (id a) (id b)))

(defmethod equiv:equiv ((a org-graph-node) (b org-graph-external-node))
  nil)

(defmethod equiv:equiv ((a org-graph-external-node) (b org-graph-node))
  nil)

(defmethod equiv:equiv ((a org-graph-external-node) (b org-graph-external-node))
  (string-equal (id a) (id b)))

(defmethod equiv:equiv ((a org-graph-node) (b org-heading))
  (when-let* ((props (org-properties b))
              (id (find "ID" (org-contents props) :test 'string-equal :key 'name)))
    (uuid= (id a) (make-uuid-from-string (value id)))))

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

(defun path= (a b) (unless (or (consp a) (consp b)) (string= (namestring (path a)) (namestring (path b)))))

;; TODO 2025-10-07: goal of this function is to expand headings and return
;; edges - the edges we're targeting right now are parent/child relationships
;; which can be auto-inferred based on ORG-STARS of HEADLINE and POINT of NODE.

;; may want to make batches per file.
(defun expand-headings (&optional (headings *org-graph-headings*) (edges *org-graph-edges*))
  (loop for h in headings
        do (org-stars (org-headline h))
        finally (return edges)))

(defvar *org-graph-headings* nil)

(defun expand-nodes (&optional (nodes *org-graph-nodes*) (files *org-graph-files*))
  "Expand a list of NODES, returning a list of newly discovered edges."
  ;; ensure files are collected
  (flet ((.find (n) (find n files :test 'path=)))
    (let ((p (.find (car nodes))))
    (loop for x in nodes
          with hl
          unless (path= x p) do (setf p (.find x))
          do (path p)
          do (push (find x (org-graph-file-tree p) :test 'equiv:equiv) hl)
          finally (return (nreverse hl))))))

(defun expand-graph ()
  (setf *org-graph-headings* (expand-nodes *org-graph-nodes*)
        *org-graph-nodes* (expand-edges *org-graph-edges*)
        *org-graph-edges* (expand-headings *org-graph-headings*)))

;;; Files
(defstruct org-graph-file 
  "Internal helper struct used while processing files in the *ORG-GRAPH*."
  path tree timestamp hash)

(defaccessor path ((self org-graph-file)) (org-graph-file-path self))

(defvar *org-graph-files* nil)

(defun org-graph-extract-files (&optional (nodes *org-graph-nodes*))
  (let ((ret))
    (std/async::dosequence (n (remove-duplicates nodes :test 'string= :key 'path) ret)
      (push (wrap (make-org-graph-file) (probe-file (path n))) ret))))

(defmethod id ((self org-graph-file))
  (org-graph-file-hash self))

(defmethod wrap ((self org-graph-file) (file pathname))
  (setf (org-graph-file-hash self) (octet-vector-to-hex-string (crypto:digest-file (crypto:make-digest :md5) file))
	(org-graph-file-path self) file
	(org-graph-file-timestamp self) (universal-to-timestamp (file-write-date file))
	(org-graph-file-tree self) (doc-tree (organ:org-parse :document file)))
  self)

(defmethod wrap ((self org-graph-file) (node org-graph-node))
  (let ((file (path node)))
    (setf (org-graph-file-hash self) (octet-vector-to-hex-string (crypto:digest-file (crypto:make-digest :md5) file))
	  (org-graph-file-path self) file
	  (org-graph-file-tree self) (doc-tree (organ:org-parse :document file)))
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
	     (when-let ((id (id h)))
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

;; Json output is needed for web UI (JS)
(defmethod json:json-write ((self org-graph-node) &optional stream)
  (let ((obj (make-hash-table :test 'equal)))
    (dolist (x '("name" "path" "point" "id"))
      (setf (gethash x obj) (slot-value self (intern (string-upcase x) :organ/graph))))
    (when (slot-boundp self 'properties)
      (setf (gethash "properties" obj) (plist-string-hash-table (node-properties self))))
    (json:json-write obj stream)))

(defmethod json:json-write ((self org-graph-edge) &optional stream)
  (let ((obj (make-hash-table :test 'equal)))
    (dolist (x '("type" "timestamp" "in" "out" "point"))
      (setf (gethash x obj) (slot-value self (intern (string-upcase x) :organ/graph))))
    (when (slot-boundp self 'properties)
      (setf (gethash "properties" obj) (plist-string-hash-table (edge-properties self))))
    (json:json-write obj stream)))

(definline %org-property-drawer-hash-table (self)
  (when self
    (let ((tbl (make-hash-table :test 'equal)))
      (sb-int:dovector (i (org-contents self) tbl)
        (setf (gethash (string-downcase (name i)) tbl) (value i)))
      tbl)))

(definline %org-heading-hash-table (self)
  (let ((obj (make-hash-table :test 'equal)))
    (setf (gethash "contents" obj) (org-contents (org-contents (org-contents self)))) ; section -> paragraph -> string
    (setf (gethash "title" obj) (slot-value (org-headline self) 'organ::title))
    (setf (gethash "properties" obj) (%org-property-drawer-hash-table (organ::org-properties self)))
    (setf (gethash "tags" obj) (map 'list 'name (slot-value (org-headline self) 'tags)))
    obj))

(defmethod json:json-write ((self org-heading) &optional stream)
  (json:json-write (%org-heading-hash-table self) stream))
  
(defmethod serialize ((self org-graph) (format (eql :json)) &key stream path)
  (if stream
      (let ((obj (make-hash-table :test 'equal)))
        (setf (gethash "nodes" obj) *org-graph-nodes*
              (gethash "links" obj) *org-graph-edges*)
        (json:json-write obj stream))
      (with-open-file (f path :direction :output :external-format :utf-8)
        (serialize self :json :stream f))))

(defun %fix-path (obj root &optional id)
  (merge-uris
   (let* ((dir (pathname-directory (path obj)))
          (str
            (concatenate 'string
                         (namestring
                          (make-pathname :name (pathname-name (path obj))
                                         :type nil
                                         :directory (cons :relative (cdr (member "graph" dir :test 'equal))))))))
     (if id
         (concatenate 'string str "#" id)
         str))
   root))
                     
  
(defun org-graph-node-fix-path (node root)
  (when (and (not (uri-p (path node))) (absolute-pathname-p (path node))) ; only apply to local pathnames
    (setf (path node) (%fix-path node root (string-downcase (uuid-to-string (id node)))))
    node))

(defun org-graph-file-fix-path (file root)
  (unless (uri-p (path file))
    (setf (path file) (%fix-path file root))
    (map nil (lambda (x) 
               (when-let ((props (organ::org-properties x)))
                 (vector-push-extend 
                  (org-create :node-property 
                              :name "path" 
                              :value (if-let ((id (id x)))
                                       (let ((u (copy-uri (path file))))
                                         (setf (uri-fragment u) (string-downcase (value id)))
                                         u)
                                       (path file)))
                  (org-contents props))))
         (org-graph-file-tree file))))

(defun org-graph-node-fix-paths (&optional (nodes *org-graph-nodes*) (root "https://otom8.dev/graph/"))
  (mapc (lambda (x) (org-graph-node-fix-path x root)) nodes))

(defun org-graph-file-fix-paths (&optional (files *org-graph-files*) (root "https://otom8.dev/graph/"))
  (mapc (lambda (x) (org-graph-file-fix-path x root)) files))

(defun org-graph-json (&optional (graph *org-graph*))
  "Generate a json object containing the nodes and edges of GRAPH."
  (org-graph-node-fix-paths)
  (serialize graph :json :path "/opt/stash/data/web/cdn/data/org-graph.json"))

(defun org-graph-index (&optional (files *org-graph-files*))
  "Generate a json search index based on FILES."
  (org-graph-file-fix-paths)
  (serialize (flatten (mapcar (lambda (x) (coerce (org-graph-file-tree x) 'list)) files)) :json
             :path "/opt/stash/data/web/cdn/data/org-graph-index.json"))
