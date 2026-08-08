;;; graph.lisp --- Org Graph Lisp API

;; 

;;; Code:
(in-package :organ/graph)

(load-database-backend :rdb)

(defvar *org-graph-file* (merge-pathnames ".config/emacs/graph.sxp" (user-homedir-pathname)))

;;; Org Graph
(defvar *org-graph* nil)
(defvar *org-graph-nodes* nil)
(defvar *org-graph-edges* nil)
(defvar *org-graph-files* nil)

(defclass org-graph (directed-graph) ()
  (:default-initargs 
   :nodes (make-array 0 :element-type 'node :adjustable t :fill-pointer t)
   :edges (make-array 0 :element-type 'edge :adjustable t :fill-pointer t)))

(defmethod read-ast ((fmt (eql :org-graph)) stream)
  "Read an ORG-GRAPH specification from STREAM."
  (let* ((ast (read stream))
         (nodes (mapcar 'wrap-node (getf ast :nodes)))
         (graph (make-instance 'org-graph :nodes nodes))
         (edges (mapcar (lambda (x) (add-edge graph (wrap-edge x))) (getf ast :edges))))
    (values graph nodes edges)))

(defmethod build ((self org-graph) &key)
  `(:nodes ,(mapcar 'build (nodes self))
    :edges ,(mapcar 'build (edges self))))

(defun read-org-graph-file (&optional (file *org-graph-file*))
  (with-open-file (f file) 
    (read-ast :org-graph f)))

(defun init-org-graph ()
  (multiple-value-bind (graph nodes edges) (read-org-graph-file)
    (setf *org-graph* graph)
    (setf *org-graph-nodes* (sort (copy-list nodes) #'string< :key (lambda (x) (namestring (path x))))
          *org-graph-edges* edges)
    (setf *org-graph-files* (org-graph-extract-files))
    (expand-graph)
    graph))

(defmethod init ((self (eql :org-graph)) &key)
  (init-org-graph))

(defun org-graph-stats ()
  `(:node-count ,(length *org-graph-nodes*)
    :edge-count ,(length *org-graph-edges*)
    :file-count ,(length *org-graph-files*)))

(defclass org-graph-node (vertex) 
  ((name :initarg :name :accessor name) 
   (path :initarg :path :accessor path)
   (point :initarg :point :accessor idx)
   (properties :initarg :properties :accessor node-properties)))

(defclass org-graph-external-node (org-graph-node) ())

(defclass org-graph-file-node (org-graph-node ast) ()
  (:default-initargs :point 0))
  
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

(defmethod build ((self org-graph-node) &key)
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
  (:documentation "A graph edge which is inferred and added to the graph as the result of a graph
expansion. See EXPAND-FILES.")
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

(defmethod build ((self org-graph-edge) &key)
  `(,(keywordicate (edge-type self)) ,(uuid-to-string (edge-in self)) ,(edge-properties self)
    ,(timestamp-to-universal (timestamp self)) ,(idx self) ,(format nil "~A" (edge-out self))))

(defmethod equiv ((a org-graph-node) (b org-graph-node))
  (uuid= (id a) (id b)))

(defmethod equiv ((a org-graph-node) (b org-graph-external-node))
  nil)

(defmethod equiv ((a org-graph-external-node) (b org-graph-node))
  nil)

(defmethod equiv ((a org-graph-external-node) (b org-graph-external-node))
  (string-equal (id a) (id b)))

(defmethod equiv ((a org-graph-file-node) (b org-graph-file-node))
  (and (path a) (path b) (path= a b)))

(defmethod equiv ((a org-graph-file-node) (b t))
  nil)

(defmethod equiv ((a org-graph-node) (b org-heading))
  (when-let* ((a (id a))
              (b (id b)))
    (when (stringp b) (setf b (make-uuid-from-string b)))
    (and (typep a 'uuid)
         (uuid= a b))))

(defmethod equiv ((a org-heading) (b org-graph-node))
  (when-let* ((a (id a))
              (b (id b)))
    (when (stringp a) (setf a (make-uuid-from-string a)))
    (and (typep b 'uuid)
         (uuid= a b))))

(defmethod equiv ((a org-graph-edge) (b org-graph-edge))
  (and (equal (edge-in a) (edge-in b))
       (equal (edge-out a) (edge-out b))))

(defun expand-edges (&optional (edges *org-graph-edges*) (nodes *org-graph-nodes*))
  "Expand a list of EDGES, returning a list of newly discovered nodes."
  (dolist (x edges nodes)
    (when (eql :relation (edge-type x))
      (let ((i (edge-out x)))
        (unless (typep i 'uuid) ;; if it's a uuid it's already a node
          (pushnew
           (extract-external-node x)
           nodes
           :test 'equiv))))))

(defun path= (a b) (unless (or (consp a) (consp b)) (string= (namestring (path a)) (namestring (path b)))))

(defun expand-nodes (&optional (nodes *org-graph-nodes*) (files *org-graph-files*))
  "Expand a list of NODES, returning a list of newly discovered edges."
  ;; ensure files are collected
  (flet ((.find (n) (find n files :test 'path=)))
    (let ((p (.find (car nodes))))
    (loop for x in nodes
          with hl
          unless (path= x p) do (setf p (.find x))
          do (path p)
          do (push (find x (ast p) :test 'equiv) hl)
          finally (return (nreverse hl))))))

(defun expand-graph ()
  (multiple-value-setq (*org-graph-nodes* *org-graph-edges*)
    (expand-files *org-graph-files* (expand-edges *org-graph-edges*) *org-graph-edges*)))

;;; Files
(defun org-graph-extract-files (&optional (nodes *org-graph-nodes*))
  (let ((ret))
    (dosequence (n (remove-duplicates nodes :test 'string= :key 'path) ret)
      (push (wrap (make-instance 'org-graph-file-node) (probe-file (path n))) ret))))

(defun %file-keywords (file doc)
  (let ((props))
    (loop for m across (org-keywords (doc-meta doc))
          do (progn (push (val m) props) (push (keywordicate (string-upcase (name m))) props)))
    (push (universal-to-timestamp (file-write-date file)) props)
    (push :timestamp props)
    props))

(defmethod wrap ((self org-graph-file-node) (file pathname))
  (let ((doc (org-parse :document file)))
    (setf (id self) (octet-vector-to-hex-string (crypto:digest-file (crypto:make-digest :md5) file))
	  (path self) file
	  (ast self) (ast doc)
          (name self) (org-title doc)
          (node-properties self) (%file-keywords file doc))
    self))

(defmethod wrap ((self org-graph-file-node) (node org-graph-node))
  (let* ((file (path node))
         (doc (org-parse :document file)))
    (setf (id self) (octet-vector-to-hex-string (crypto:digest-file (crypto:make-digest :md5) file))
	  (path self) file
	  (ast self) (ast doc)
          (name self) (org-title doc)
          (node-properties self) (%file-keywords file doc))
    self))

(defun expand-files (&optional (files *org-graph-files*) (nodes *org-graph-nodes*) (edges *org-graph-edges*))
  (mapc 
   (lambda (x) 
     (when x
       (pushnew x nodes :test 'equiv)
       (let (stack)
         (flet ((.push (a b)
                  (pushnew
                   (make-instance 'org-graph-implicit-edge
                     :type :child 
                     :in (or (id a) (id x)) :out (or (id b) (id x))
                     :timestamp (time:now)
                     :point (or (idx a) (idx b)))
                   edges
                   :test 'equiv)))
           (loop for h across (ast x)
                 if (or (null stack) (< (org-stars h) (org-stars (car stack))))
                 do (progn 
                      (push h stack) 
                      (.push x h))
                 else if (> (org-stars h) (org-stars (car stack)))
                 do (progn 
                      (push h stack) 
                      (.push (cadr stack) (car stack)))
                 else do (progn 
                           (setf (car stack) h)
                           (.push (or (cadr stack) x) h)))))))
   files)
  (values nodes edges))

;; (defun expand-headings (&optional (files *org-graph-files*) (edges *org-graph-edges*)))

;;; Serde
(defmethod serialize ((self org-graph) format &key stream)
  (serialize (build self) format :stream stream))

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
        (unless (or (null i) (null (value i)))
          (setf (gethash (string-downcase (name i)) tbl) (value i))))
      tbl)))

(defvar *org-graph-export-id* -1)

(defvar *org-graph-export-identity*
    (lambda (id) (declare (ignore id)) (incf *org-graph-export-id*))
  "A function which takes a single argument (the ID of an object) and returns a
new ID to be serialized on export.")

(definline %org-heading-hash-table (self identity)
  (let ((obj (make-hash-table :test 'equal)))
    (setf (gethash "id" obj) (funcall identity (id self)))
    (setf (gethash "title" obj) (org-title (org-headline self)))
    (setf (gethash "tags" obj) (map 'list 'name (org-tags (org-headline self))))
    (setf (gethash "properties" obj) (%org-property-drawer-hash-table (org-properties self)))
    (setf (gethash "contents" obj) (org-contents (org-contents (org-contents self)))) ; section -> paragraph -> string
    obj))

(defmethod json:json-write ((self org-heading) &optional stream)
  (json:json-write (%org-heading-hash-table self *org-graph-export-identity*) stream))
  
(defmethod serialize ((self org-graph) (format (eql :json)) &key stream path if-exists (if-does-not-exist :create) (external-format :default))
  (if stream
      (let ((obj (make-hash-table :test 'equal)))
        (setf (gethash "nodes" obj) *org-graph-nodes*
              (gethash "links" obj) *org-graph-edges*)
        (json:json-write obj stream))
      (with-open-file (f path :direction :output 
                              :external-format external-format
                              :if-does-not-exist if-does-not-exist 
                              :if-exists if-exists)
        (serialize self :json :stream f))))

(defun %fix-path (obj root &optional id)
  (merge-uris
   (let* ((dir (pathname-directory (path obj)))
          (str
            (concatenate 
             'string
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
    (setf (path node) (%fix-path node root (unless (typep node 'org-graph-file-node) 
                                             (string-downcase (uuid-to-string (id node))))))
    node))

(defun org-graph-node-fix-paths (&optional (nodes *org-graph-nodes*) (root "https://compiler.company/graph/"))
  (mapc (lambda (x) (org-graph-node-fix-path x root)) nodes))

(defun org-graph-json (&key (graph *org-graph*) (if-exists :supersede) path)
  "Generate a json object containing the nodes and edges of GRAPH."
  (org-graph-node-fix-paths)
  (serialize graph :json :path path :if-exists if-exists))

(defun org-graph-minisearch-json (&key (files *org-graph-files*) (if-exists :supersede) path)
  "Generate a Minisearch json search index based on FILES."
  (let ((*org-graph-export-id* -1))
    (serialize (flatten (mapcar (lambda (x) (when x (coerce (ast x) 'list))) files)) :json
               :path path
               :if-exists if-exists)))

(defun org-graph-tinysearch-json (&key (files *org-graph-files*) (if-exists :supersede) path)
  "Generate a Tinysearch json search index based on FILES."
  (serialize 
   (mapcar (lambda (x)
             (let ((tbl (make-hash-table :test 'equal)))
               (setf (gethash "title" tbl) (org-title (org-headline x))
                     (gethash "url" tbl) (when-let ((y (find x *org-graph-nodes* :test 'equiv))) (path y))
                     (gethash "body" tbl) (org-contents (org-contents (org-contents x))))
               tbl))
           (flatten (mapcar (lambda (x) (when x (coerce (ast x) 'list))) files)))
   :json
   :path path
   :if-exists if-exists))
