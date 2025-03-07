;;; graph.lisp --- Org Graph Lisp API

;; 

;;; Code:
(in-package :organ/graph)

(load-database-backend :rdb)
(blake3:load-blake3)

(deftype org-id () `(octet-vector 16))

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

(defvar *org-graph-db-directory* (merge-pathnames ".store/db/graph/" (user-homedir-pathname)))

(defun make-org-graph-db ()
  (load-schema
   (make-db :rdb :name (namestring *org-graph-db-directory*)
		 :opts (default-rdb-opts))
   *org-graph-schema*))

(defvar *org-graph-db* (make-org-graph-db))

(define-condition org-id-locations-out-of-sync (simple-error) ())

(defvar *org-graph-file* (merge-pathnames ".emacs.d/graph.sxp" (user-homedir-pathname)))
(defvar *org-id-locations-file* (merge-pathnames ".emacs.d/.org-id-locations" (user-homedir-pathname)))

(defun make-org-id-locations (&optional (file *org-id-locations-file*))
  (let ((tbl (make-hash-table :test 'equal)))
    (with-open-file (file file)
      (dolist (entry (read file))
	(if-let ((file (probe-file (car entry))))
	  (setf (gethash (namestring file) tbl) (cdr entry))
	  (signal 'org-id-locations-out-of-sync :format-control "~A" :format-arguments (list entry)))))
    tbl))

(defun uuid-octets* (id)
  (handler-case (uuid-to-octet-vector id)
    (simple-error () id)
    (sb-pcl::missing-slot () id)))

(defvar *org-id-locations* (make-org-id-locations))

(defvar *org-graph* nil)

(defstruct org-graph nodes edges)

(defmethod read-ast ((fmt (eql :org-graph)) stream &key)
  (let ((graph (apply 'make-org-graph (read stream))))
    (setf #1=(org-graph-nodes graph) 
	  (mapcar (lambda (x) (wrap (make-instance 'org-graph-node) x)) #1#)
	  #2=(org-graph-edges graph) 
	  (mapcar (lambda (x) (wrap (make-instance 'org-graph-edge) x)) #2#))
    graph))

(defun read-org-graph-file (&optional (file *org-graph-file*))
  (with-open-file (f file) (read-ast :org-graph f)))

(defun init-org-graph ()
  (setf *org-graph* (read-org-graph-file)))

(defclass org-graph-node (vertex) 
  ((name :initarg :name :accessor name) 
   (path :initarg :path :accessor path)
   (point :initarg :point :accessor idx)))

(defmethod wrap ((self org-graph-node) form)
  (make-instance 'org-graph-node 
    :id (make-uuid-from-string (pop form)) :name (pop form) :path (pop form) :point (pop form)))

(defclass org-graph-edge (edge) 
  ((type :initarg :type :accessor edge-type)
   (properties :initarg :properties :accessor edge-properties)
   (timestamp :initarg :timestamp :accessor timestamp)
   (point :initarg :point :accessor idx)))

(defmethod wrap ((self org-graph-edge) form)
  (make-instance 'org-graph-edge
    :type (pop form) 
    :in (make-uuid-from-string (pop form))
    :properties (pop form) 
    :timestamp 
    (destructuring-bind (sec minute hour day month year timezone a1 a2) (pop form)
      (declare (ignore a1 a2))
      (encode-timestamp 0 sec minute hour day month year :timezone (or timezone *default-timezone*)))
    :point (pop form)
    :out (pop form)))

(defstruct org-graph-file 
  "Internal helper struct used while processing files in the *ORG-GRAPH*."
  path document timestamp hash)

(defun org-graph-extract-files (&optional (graph *org-graph*))
  (let ((ret))
    (dolist (n (remove-duplicates (org-graph-nodes graph) :test 'string= :key 'path) ret)
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

(defun insert-org-files ()
  (log:info! "inserting org files")
  (mapcar 
   (lambda (n) (insert-key *org-graph-db* (uuid-octets* (id n)) (path n) :column "file"))
   (org-graph-nodes *org-graph*)))

(defun insert-org-nodes ()
  (log:info! "inserting org nodes")
  (dolist (id (mapcar 'id (org-graph-nodes *org-graph*)))
    (insert-key *org-graph-db*
		(uuid-octets* id)
		;; TODO 2024-12-30: 
		#(1)
		:column "node")))

(defun insert-org-edges ()
  (log:info! "inserting org edges")
  (dolist (e (org-graph-edges *org-graph*))
    (insert-key *org-graph-db* 
		(uuid-octets* (edge-in e)) 
		(uuid-octets* (edge-out e))
		:column "edge")))

;; (loop with i = 0
;; while (iter-valid-p it)
;; do (log:info! (iter-key it) (iter-val it))
;; do (iter-next it)
;; do (print (incf i))))

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
  (unless (probe-file *org-graph-db-directory*)
    (init-org-graph-db))
  (if (and *org-graph-db* (db-open-p *org-graph-db*))
      *org-graph-db*
      (progn
	(load-opts *org-graph-db*)
	(open-columns* *org-graph-db*))))

(defun destroy-org-graph-db ()
  (unless (db-closed-p *org-graph-db*)
    (shutdown-db *org-graph-db*)
    (destroy-db *org-graph-db*)
    (log:info! "destroyed org-graph-db at ~A" *org-graph-db-directory*)))

(defun og-get (key &optional (from "node"))
  (get-val *org-graph-db* key :data-type 'string :column from))

(defun org-graph-values (column)
  (with-iter (it (iter *org-graph-db* :column (find-column column *org-graph-db*)))
    (seek-to-first)
    (loop while (iter-valid-p)
	  collect (cons (handler-case (octet-vector-to-uuid (key))
			  (simple-type-error () (sb-ext:octets-to-string (key))))
			(sb-ext:octets-to-string (val)))
	  do (next))))

(defun org-graph-file-scrape (path &rest ids)
  "Return a list of org headings corresponding to IDS in PATH."
  ;; first get an org-document and list of headings
  (let* ((doc (organ:org-parse :document path))
	 (headings (organ:doc-tree doc))
	 (ret))
    ;; map over IDs, searching for matches
    (loop for h across headings
	  if (typep h 'organ:org-heading)
	  do
	     (push
	      (when-let* ((prop (organ::org-properties h))
			  (id (find (print (value (find "ID" (print (organ:org-contents prop))
							:key (lambda (x) (string-upcase (name x))))))
				    ids
				    :test 'equal)))
		(removef ids id :test 'equal)
		h)
	      ret)
	  finally (return ret))))
