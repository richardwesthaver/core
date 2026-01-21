;;; project.lisp --- Skel Project

;; 

;;; Code:
(in-package :skel/core)

;;; Project
(defclass sk-project (skel sk-meta simple-project)
  ((name :initarg :name :initform (format nil "~A" (gensym "SK")) :type simple-base-string :accessor name
         :documentation "The name of this project.")
   (vc :initarg :vc :initform (vc-init *default-vc-kind*) 
       :type vc-repo :accessor vc)
   (src :initarg :src :type pathname :accessor src)
   (stash :initarg :stash :accessor stash :initform ".stash/")
   (store :initarg :store :accessor store :initform ".stash/store/")
   (cache :initarg :store :accessor cache :initform ".stash/cache/")
   (components :initform #() :initarg :components :accessor components :type (vector sk-component)
               :documentation "A vector of child components belonging to this project.")
   (bind :initarg :bind :initform *default-skel-bindings* :accessor bind :type list
         :documentation "A list of dynamic bindings which are applied to rule definitions.")
   (phases :initarg :phases
	   :initform (make-hash-table)
	   :accessor phases
	   :type hash-table
           :documentation "A hash-table containing PHASE-NAME : RULE-MEMBER-LIST pairs.")
   (rules :initarg :rules
	  :initform (make-array 0 :element-type 'sk-rule :adjustable t)
	  :accessor rules
	  :type (vector sk-rule)
          :documentation "A vector of rule objects containing individual units of work. Each rule is
implicitly linked to a phase in the PHASES hash-table slot.")
   (include :initarg :include
	    :initform (make-array 0 :element-type 'pathname :adjustable t)
	    :accessor include
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
	    (length (components self))
	    (length (rules self)))))

(defmethod sk-new ((self (eql :project)) &rest args)
  (declare (ignore self))
  (apply #'sk-new 'sk-project args))

(defun find-sk-symbol (s)
  (find-symbol* (symbol-name s) :skel/core t))

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
  (with-skel-ast ast self
    ;; ast is valid, modify object, set ast nil
    (progn
      (sb-int:doplist (k v) ast
	(when-let ((s (find-sk-symbol k)))
	  (setf (slot-value self s) v))) ;; needs to be correct package
	  ;;; SRC
      (if (bound-string-p self 'src)
	  (setf (src self) (or (probe-file (src self))
				  (probe-file (merge-pathnames (src self) *skel-path*))
				  (error 'invalid-argument :reason "project source not found"
							   :item (src self))))
	  (setf (src self) (sk-dir self)))
      (setq *skel-path* (or (src self) *default-pathname-defaults*))
      (let ((*default-pathname-defaults* (make-pathname :defaults (namestring *skel-path*))))
	(when (bound-string-p self 'stash) 
          (setf (stash self) (ensure-directory-truename (the simple-string (stash self)))))
        (when (bound-string-p self 'store) 
          (setf (store self) (ensure-directory-truename (the simple-string (store self)))))
        (when (bound-string-p self 'cache)
          (setf (cache self) (ensure-directory-truename (the simple-string (cache self)))))
	;; VC
	(when-let ((vc (vc self)))
	  (etypecase vc
	    ((or vc-repo null) nil)
	    (vc-designator (setf (vc self) (vc-init vc)))
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
                        (when (eql (vc-type repo) :hg)
                          (setf (vc/hg::vc-bookmarks repo) (find-hg-bookmarks (path repo))
                                (vc/hg::vc-requires repo) (vc/hg:find-hg-requires (path repo))
                                (vc-submodules repo) (vc/hg::find-hg-submodules (path repo))))
			repo)))
	       (setf (vc self) (%vc-scan vc))))))
	;; INCLUDE
	(when-let ((include (include self)))
	  (setf (include self) 
                (map 'vector
		     ;; recursively load included projects
		     (lambda (i) 
                       (load-ast
			(sk-read-file
			 (make-instance 'sk-project)
			 i)))
		     include)))
	;; COMPONENTS
	(when (slot-boundp self 'components)
	  (setf (components self) (map 'vector
					  (lambda (c)
					    (sk-load-component
					     (pop c)
                                             (if (= 1 (length c))
                                                 (pathname (car c))
                                                 c)
					     *default-pathname-defaults*))
					  (components self)))))
      ;; BIND contains a list of forms which are bound dynamically based
      ;; on the contents of the cdr
      (when-let ((bind (bind self)))
	(setf (bind self)
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
      (when-let ((rules (rules self)))
	(setf (rules self)
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
				 (let ((ph (gethash phase (phases self))))
				   (setf (gethash phase (phases self))
					 (push (make-sk-rule %target source recipe) ph))))))
			   recipe))
			 (make-sk-rule target source recipe))))
		 (coerce rules 'list)))
	       '(vector sk-rule))))          
      (unless *keep-ast* (setf (ast self) nil))
      (setf (id self) (sxhash (cons (name self) (version self))))
      self)))

;; obj -> ast

;; need to define a method for SK-PROJECT to add PHASES to the exclusion list.
(defmethod build-ast ((self sk-project) &key (nullp nil) (exclude '(ast id phases)))
  (setf (ast self)
        (unwrap-object self
                       :slots t
                       :methods nil
                       :nullp nullp
                       :exclude exclude))
  self)

;; file -> ast
(defmethod sk-read-file ((self sk-project) path)
  (wrap self (file-read-forms path))
  (setf (path self) (ensure-absolute-pathname path *default-pathname-defaults*))
  self)

;; ast -> file
(defmethod sk-write-file ((self sk-project) 
			  &key 
			  (path *default-skelfile*) (nullp nil) (comment t) (pretty t)
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
		        :description (description self)
		        :opts '("mode:skel;"))
		       out))
	(write-ast self out :pretty pretty))
    (unless *keep-ast* (setf (ast self) nil))))

(defmethod wrap ((self sk-project) (config sk-user-config))
  (with-slots (vc store stash license author) (debug! config) ;; log-level, custom, fmt
    (setf (vc self) vc)
    (setf (stash self) stash)
    (setf (store self) store)
    (setf (license self) license)
    (setf (author self) author)))

(defmethod sk-find ((item sk-rule) (self skel) &key)
  (find (string-upcase (sk-rule-target item))
	(rules self) :test 'string-equal :key 'sk-rule-target))

(defmethod sk-find ((item t) (self skel) &key)
  (find (string-upcase item) (rules self) :test 'string-equal :key #'sk-rule-target))

(defmethod sk-find ((name string) (self sk-config) &key)
  (find name (scripts self) :test 'equal :key #'name))

(defmethod sk-call ((self sk-project) (arg sk-rule))
  (sk-make self arg))

(defmethod sk-call ((self sk-project) (arg t))
  (sk-make self (sk-find arg self)))

(defmethod sk-call ((self sk-project) (arg (eql :compile)))
  (loop for c across (components self)
	collect (sk-compile self)))

(defmethod sk-call ((self sk-project) (arg (eql :build)))
  (loop for c across (components self)
	collect (sk-build self)))

(defmethod sk-call ((self sk-project) (arg (eql :load)))
  (loop for c across (components self)
	collect (sk-load self)))

(defmethod sk-call ((self sk-project) (arg (eql :clean)))
  (if-let ((x (sk-find arg self)))
    (sk-make self x)
    (funcall skel/core::*default-clean-function* self)))

(defmethod sk-build ((self sk-project) &key)
  (loop for c across (components self)
	collect (sk-build c)))

(defmethod sk-compile ((self sk-project) &key)
  (loop for c across (components self)
	collect (sk-compile c)))

(defmethod sk-load ((self sk-project) &key)
  (loop for c across (components self)
	collect (sk-load c)))
