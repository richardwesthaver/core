;;; project.lisp --- Skel Project

;; 

;;; Code:
(in-package :skel/core/obj)

;;; Project
(defclass sk-project (skel ast sk-meta)
  ((name :initarg :name :initform (format nil "~A" (gensym "SK")) :type simple-base-string :accessor name
         :documentation "The name of this project.")
   (vc :initarg :vc :initform (vc-init *default-skel-vc-kind*) 
       :type vc-repo :accessor sk-vc)
   (src :initarg :src :type pathname :accessor sk-src)
   (stash :initarg :stash :accessor sk-stash :type pathname)
   (store :initarg :store :accessor sk-store :type pathname)
   (components :initform #() :initarg :components :accessor sk-components :type (vector sk-component)
               :documentation "A vector of child components belonging to this project.")
   (bind :initarg :bind :initform *default-skel-bindings* :accessor sk-bind :type list
         :documentation "A list of dynamic bindings which are applied to rule definitions.")
   (phases :initarg :phases
	   :initform (make-hash-table)
	   :accessor sk-phases
	   :type hash-table
           :documentation "A hash-table containing PHASE-NAME : RULE-MEMBER-LIST pairs.")
   (rules :initarg :rules
	  :initform (make-array 0 :element-type 'sk-rule :adjustable t)
	  :accessor sk-rules
	  :type (vector sk-rule)
          :documentation "A vector of rule objects containing individual units of work. Each rule is
implicitly linked to a phase in the PHASES hash-table slot.")
   (include :initarg :include
	    :initform (make-array 0 :element-type 'pathname :adjustable t)
	    :accessor sk-include
	    :type (vector pathname)
            :documentation "A list of skelfiles to include in the current project. Files in this list may
define their own subprojects or extend the current one."))
  (:documentation "Skel project base class, usually defined by skelfiles at a project's root
directory."))

(defmethod print-object ((self sk-project) stream)
  (print-unreadable-object (self stream)
    (format stream "~A ~A :components ~A :rules ~A"
	    (sk-class-name self t)
	    (name self)
	    (length (sk-components self))
	    (length (sk-rules self)))))

(defmethod sk-new ((self (eql :project)) &rest args)
  (declare (ignore self))
  (apply #'sk-new 'sk-project args))

(defun find-sk-symbol (s)
  (find-symbol* (symbol-name s) :skel/core/obj t))

(defun %recipe-phase-p (form)
  "Return non-nil if FORM looks like (:PHASE &BODY BODY)."
  (and (listp form) (>= (length form) 2) (keywordp (car form))))

(defun sk-multi-recipe-p (recipe)
  "Return T if RECIPE looks like a list of (:PHASE &BODY BODY)."
  (when (consp recipe)
    (every '%recipe-phase-p recipe)))

(defun sk-case-bind (key val &optional sym)
  "Switch on keyword KEY, evaluating a skel binding."
  (case key
    (:dir-locals
     ;; nothing actually needs to be done here, the value itself can be parsed
     ;; directly from emacs via sk.el package. For convenience, when SYM is
     ;; present we bind it to the list of variables.
     (when sym (list sym val)))
    (:hook
     ;; process the remainder of the form as specializer+body
     (destructuring-bind (spec &rest body) val
       (declare (ignore spec body))
       (nyi!)))
    (:macro
     (destructuring-bind (args &rest body) val
       (push (list sym args body) *skel-project-macros*)))
    (:symbol-macro
     (push (list sym val) *skel-project-symbol-macros*))
    (:function
        (destructuring-bind (args &rest body) val
          (push (list sym args body) *skel-project-functions*)))
    ;; (:cmd
    ;;  ;; process the remainder as spec+defcmd-args+body
    ;;  )
    ;; (:opt
    ;;  ;; process the remainder as spec+defopt-args+body
    ;;  )
    (:env
     ;; process the remainder as a regular value but
     ;; associate the name with a shell environment which
     ;; is set to the value. If the cdr is of length 3
     ;; then we simply remember the value and set it during
     ;; any calls out from Lisp to the shell. When the form
     ;; length is > 3 we parse the next value as a shell
     ;; specification with additional options for checking
     ;; for pre-existing values and 'exporting' the
     ;; environment.
     (unless (null val)
       (let ((val (if (listp val) (eval val) val))
	     (_sym (substitute #\_ #\- (string sym))))
	 (setf (uiop:getenv _sym) (format nil "~A" val))
	 (log:trace! "env: ~A=~A~%" _sym val))))))

;; ast -> obj
(defmethod load-ast ((self sk-project))
  ;; internal ast is never tagged
  (with-slots (ast) self
    (if (formp ast)
	;; ast is valid, modify object, set ast nil
	(progn
	  (sb-int:doplist (k v) ast
	    (when-let ((s (find-sk-symbol k)))
	      (setf (slot-value self s) v))) ;; needs to be correct package
	  ;;; SRC
	  (if (bound-string-p self 'src)
	      (setf (sk-src self) (or (probe-file (sk-src self))
				      (probe-file (merge-pathnames (sk-src self) skel-path))
				      (error 'invalid-argument :reason "project source not found"
							       :item (sk-src self))))
	      (setf (sk-src self) (sk-dir self)))
	  (setq skel-path (or (sk-src self) *default-pathname-defaults*))
	  (let ((*default-pathname-defaults* (make-pathname :defaults (namestring skel-path))))
	    (when (bound-string-p self 'stash) (setf (sk-stash self) (pathname (the simple-string (sk-stash self)))))
	    (when (bound-string-p self 'store) (setf (sk-store self) (pathname (the simple-string (sk-store self)))))
	    ;; VC
	    (when-let ((vc (sk-vc self)))
	      (etypecase vc
		((or vc-repo null) nil)
		(vc-designator (setf (sk-vc self) (vc-init vc)))
		(list
		 (flet ((%vc-scan (lst)
			  (let* ((%type (if (typep (car lst) 'vc-designator)
					    (pop lst)
					    *default-vc-kind*))
				 (repo (vc-init %type)))
			    (setf (vc-remotes repo)
				  (map 'vector
				       (lambda (v)
					 (etypecase v
					   (string (vc::make-vc-remote :name 'default :url v))
					   (list 
					    (let ((name (pop v))
						  (val (pop v)))
					      (if (consp val)
						  (vc::make-vc-remote :name name
								      :type (pop val)
								      :url (pop val))
						  (vc::make-vc-remote :name name
								      :url val))))))
				       lst))
			    repo)))
		   (setf (sk-vc self) (%vc-scan vc))))))
	    ;; INCLUDE
	    (when-let ((include (sk-include self)))
	      (setf (sk-include self) (map 'vector
					   ;; recursively load included projects
					   (lambda (i) 
                                             (load-ast
					      (sk-read-file
					       (make-instance 'sk-project)
					       i)))
					   include)))
	    ;; COMPONENTS
	    (when (slot-boundp self 'components)
	      (setf (sk-components self) (map 'vector
					      (lambda (c)
						(sk-load-component
						 (pop c)
                                                 (if (= 1 (length c))
                                                     (pathname (car c))
                                                     c)
						 *default-pathname-defaults*))
					      (sk-components self)))))
	  ;; BIND contains a list of forms which are bound dynamically based
	  ;; on the contents of the cdr
	  (when-let ((bind (sk-bind self)))
	    (setf (sk-bind self)
		  (let ((ret))
		    ;; TODO 2024-09-21: 
		    (dolist (b bind ret)
		      ;; if this is a list of length > 2 we parse the form as either
		      ;; (key &rest val) or (var param &rest val)
		      (let ((sym (car b))
			    (form (cdr b)))
			;; (form (cddr b)))
			(let ((key (car form))
			      (val (if (= (length #1=(cdr form)) 1) (cadr form) #1#)))
			  (if (keywordp key)
			      (sk-case-bind key val sym)
			      (cond
				;; (sym param &rest val) detected
				((> (length (cdr form)) 0)
				 (let ((key (cadr b)))
				   (if (keywordp key)
				       (sk-case-bind key (cdr form) sym)
				       ;; if nothing else must be a lambda
				       (push `(,sym 
					       ,(compile sym `(lambda ,(car b) ,@(cddr b))))
					     ret))))
				(t
				 (push b ret))))))))))
	  ;; RULES
	  (when-let ((rules (sk-rules self)))
	    (setf (sk-rules self)
		  (coerce
		   (flatten
		    (mapcar
		     (lambda (x)
		       (destructuring-bind (target source &rest recipe) x
			 ;; TODO 2024-07-30: check for phases
			 (if (sk-multi-recipe-p recipe)
			     (flatten
			      (mapcar
			       (lambda (y)
				 (destructuring-bind (phase source &rest recipe) y
				   (let ((%target (keywordicate phase '- (string-upcase target))))
				     (let ((ph (gethash phase (sk-phases self))))
				       (setf (gethash phase (sk-phases self))
					     (push (make-sk-rule %target source recipe) ph))))))
			       recipe))
			     (make-sk-rule target source recipe))))
		     (coerce rules 'list)))
		   '(vector sk-rule))))          
	  (unless *keep-ast* (setf (ast self) nil))
	  (setf (id self) (sxhash (cons (name self) (sk-version self))))
	  self)
	;; invalid ast, signal error
	(invalid-skel-ast ast))))

;; obj -> ast
(defmethod build-ast ((self sk-project) &key (nullp nil) (exclude '(ast id phases)))
  (setf (ast self)
	(unwrap-object self
		       :slots t
		       :methods nil
		       :nullp nullp
		       :exclude exclude)))

;; TODO 2023-09-26: This belongs in AST
(defmethod write-ast ((self sk-project) stream &key (pretty t) (case :downcase) (fmt :pretty))
  (case fmt
    (:pretty
     (if (listp (ast self))
	 (with-open-stream (st stream)
	   (loop for (k v . rest) on (ast self)
		 by #'cddr
		 unless (or (null v) (null k))
		 do 
		    (write k :stream stream :pretty pretty :case case :readably t :array t :escape t)
		    (write-char #\space st)
		    (if (or (eq (type-of v) 'skel) (subtypep (type-of v) 'structure-object))
			(write-ast v stream :pretty pretty :case case)
			(write v :stream stream :pretty pretty :case case :readably t :array t :escape t))
		    (write-char #\newline st)))
	 (skel-io-error)))
    (t (write (ast self) :stream stream :pretty pretty :case case :readably t :array t :escape t))))

;; file -> ast
(defmethod sk-read-file ((self sk-project) path)
  (wrap self (file-read-forms path))
  (setf (path self) (ensure-absolute-pathname path *default-pathname-defaults*))
  ;; TODO 2024-04-18: make generic
  self)

;; ast -> file
(defmethod sk-write-file ((self sk-project) 
			  &key 
			  (path *default-skelfile*) (nullp nil) (comment t) (fmt :canonical)
			  (if-exists :error))
  (build-ast self :nullp nullp)
  (prog1 
      (with-open-file (out path
			   :direction :output
			   :if-exists if-exists
			   :if-does-not-exist :create)
	(when comment (princ
		       (make-source-header-comment
		        (name self)
		        :cchar #\;
		        :timestamp t
		        :description (sk-description self)
		        :opts '("mode:skel;"))
		       out))
	(write-ast self out :fmt fmt))
    (unless *keep-ast* (setf (ast self) nil))))

(defmethod sk-install-user-config ((self sk-project) (config sk-user-config))
  (with-slots (vc store stash license author) (debug! config) ;; log-level, custom, fmt
    (setf (sk-vc self) vc)
    (setf (sk-stash self) stash)
    (setf (sk-store self) store)
    (setf (sk-license self) license)
    (setf (sk-author self) author)))

(defmethod sk-find ((item sk-rule) (self skel) &key)
  (find (string-upcase (sk-rule-target item))
	(sk-rules self) :test 'string-equal :key 'sk-rule-target))

(defmethod sk-find ((item t) (self skel) &key)
  (find (string-upcase item) (sk-rules self) :test 'string-equal :key #'sk-rule-target))

(defmethod sk-find ((name string) (self sk-config) &key)
  (find name (sk-scripts self) :test 'equal :key #'name))

(defmethod sk-call ((self sk-project) (arg sk-rule))
  (sk-make self arg))

(defmethod sk-call ((self sk-project) (arg t))
  (sk-make self (sk-find arg self)))

(defmethod sk-call ((self sk-project) (arg (eql :compile)))
  (loop for c across (sk-components self)
	collect (sk-compile self)))

(defmethod sk-call ((self sk-project) (arg (eql :build)))
  (loop for c across (sk-components self)
	collect (sk-build self)))

(defmethod sk-call ((self sk-project) (arg (eql :load)))
  (loop for c across (sk-components self)
	collect (sk-load self)))

(defmethod sk-call* ((self sk-project) &rest args)
  (mapcar (lambda (arg) (sk-call self arg)) args))

(defmethod sk-build ((self sk-project) &key)
  (loop for c across (sk-components self)
	collect (sk-build c)))

(defmethod sk-compile ((self sk-project) &key)
  (loop for c across (sk-components self)
	collect (sk-compile c)))

(defmethod sk-load ((self sk-project) &key)
  (loop for c across (sk-components self)
	collect (sk-load c)))
