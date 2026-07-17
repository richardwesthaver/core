;;; project.lisp --- Skel Project

;; 

;;; Code:
(in-package :skel/core)

;;; Rules
(eval-always
  (defmacro with-sk-rule-env (binds &body body)
    `(let (,@(mapcar (lambda (x) 
                       ;; TODO 2026-06-17: 
                       (if (or (atom x) (> 3 (length x)))
                           x
                           (cons (car x) (cddr x))))
                     binds))
       (declare (ignorable ,@(mapcar 'car binds)))
       (symbol-macrolet ,*skel-project-symbol-macros*
         (macrolet ,*skel-project-macros*
           (labels ,*skel-project-functions*
             ,@body))))))

;; Note that EXEC directly on a rule currently does NOT touch the sources.
(defmethod exec ((self rule))
  (compile-and-eval
   `(with-sk-rule-env ,(bind *project*)
      ,@(ast self))))

;;; Project
(defcomponent skel-project (simple-project)
  ((name :initarg :name :initform (format nil "~A" (gensym "SK")) :type simple-base-string :accessor name)
   (vc :initarg :vc
       :initform nil
       :accessor vc)
   (src :initarg :src :type pathname :accessor src)
   (stash :initarg :stash :accessor stash :initform ".stash/")
   (store :initarg :store :accessor store :initform ".stash/store/")
   (cache :initarg :store :accessor cache :initform ".stash/cache/")
   (components :initform #() :initarg :components :accessor components :type (vector project-component)
               :documentation "A vector of child components belonging to this project.")
   (bind :initarg :bind :initform *default-skel-bindings* :accessor bind :type list
     :documentation "A list of dynamic bindings which are applied to rule definitions.")
   (rules :initarg :rules
	      :initform (make-array 0 :element-type 'rule :adjustable t)
	      :accessor rules
	      :type (vector rule)
          :documentation "A vector of rule objects containing individual units of work."))
  (:documentation "Skel project base class, usually defined by skelfiles at a project's root
directory.")
  (:keyword :project))

(defmethod print-object ((self skel-project) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A :components ~A :rules ~A"
	        (name self)
	        (length (components self))
	        (length (rules self)))))

(definline sk-coerce-name (name &optional (case :downcase))
  (if (eql :downcase case) (string-downcase name) (string-upcase name)))

(definline sk-coerce-sequence (seq &optional limit)
  (coerce
   (if limit
       (take limit seq)
       seq)
   'list))

(defun sk-print-slot (slot self &key (stream *standard-output*) (limit 8) (case :downcase))
  (declare (stream stream) (id self))
     (let ((name (sb-mop:slot-definition-name slot))
           (*print-case* case))
       (when (slot-boundp self name)
         (let ((val (slot-value self name))
               (name (sk-coerce-name name case)))
           (typecase val
             (string (format stream ":~A ~A~%" name val))
             (cons (unless (sequence:emptyp val) (format stream ":~A ~A~%" name val)))
             (vector (unless (sequence:emptyp val)
                       (format stream ":~A [" name)
                       (pprint-tabular stream (sk-coerce-sequence val limit) nil nil 2)
                       (force-output stream)
                       (if (and limit (> #2=(length val) #1=(the positive-fixnum limit)))
                           (format stream " ...~d]~%" (- #2# limit))
                           (format stream "]~%"))))
             (hash-table (unless (zerop (hash-table-count val))
                           (format stream ":~A {" name)
                           (pprint-tabular stream (sk-coerce-sequence (hash-table-alist val) limit)
                                           nil nil 2)
                   (if (and limit (> #4=(hash-table-count val) #3=(the positive-fixnum limit)))
                   (format stream " ...~d}~%" (- #4# limit))
                   (format stream "}~%"))))
             (t (format stream ":~A ~A~%" name val)))))))

(defun print-skel-object (self stream)
  (mapcar (lambda (slot) (sk-print-slot slot self :stream stream :limit *print-length* :case *print-case*))
          (remove-if 'print-slot-exclusion (sb-mop:class-slots (class-of self))))
  self)

(defun find-skel-symbol (s)
  (let ((s (symbol-name s)))
    (find-symbol* s :skel/core (find-symbol s))))

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
     ;; process the remainder as a regular value but associate the name with a
     ;; shell environment which is set to the value. If the cdr is of length 3
     ;; then we simply remember the value and set it during any calls out from
     ;; Lisp to the shell. When the form length is > 3 we parse the next value
     ;; as a shell specification with additional options for checking for
     ;; pre-existing values and 'exporting' the environment.
     (unless (null val)
       (let ((val (if (listp val) (eval val) val))
	         (_sym (substitute #\_ #\- (string sym))))
	     (sb-posix:setenv _sym (format nil "~A" val) 1)
	     (log:trace! "env: ~A=~A~%" _sym val))))))

;; ast -> obj
(defmethod load-ast ((self skel-project))
  ;; internal ast is never tagged
  (with-object-ast ast self
    ;; ast is valid, modify object, set ast nil
    (progn
      (sb-int:doplist (k v) ast
	    (when-let ((s (find-skel-symbol k)))
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
            ;; WARNING: slow path - recurses submodules, parses configs
	        (vc-designator (setf (vc self) (make-repo *skel-path* :type vc)))
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
	    ;; REQUIRE
	    (when-let ((req (slot-boundp! self 'require)))
	      (setf (module-require self)
                (mapcar ; recursively load included projects
		         (lambda (r) 
                   (load-ast
			        (read-ast
			         (make-instance 'skel-project)
			         r)))
		         req)))
	    ;; COMPONENTS
	    (when (slot-boundp self 'components)
	      (setf (components self) (map 'vector
					                   (lambda (c)
					                     (load-project-component
					                      (pop c)
                                          (if (= 1 (length c))
                                              (pathname (car c))
                                              c)
					                      :path *default-pathname-defaults*))
					                   (components self)))))
      ;; BIND contains a list of forms which are bound dynamically based
      ;; on the contents of the cdr
      (when-let ((bind (bind self)))
		(dolist (b bind)
          ;; FIX 2026-05-08: protect against use of eval?
          ;; WARN 2026-05-08: use of eval
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
			           (when (keywordp key)
				         (sk-case-bind key (cdr form) sym))))))))))
      ;; RULES
      (when-let ((rules (rules self)))
	    (setf (rules self)
	          (coerce
	           (flatten
		        (mapcar
		         (lambda (x)
		           (destructuring-bind (target source &rest recipe) x
		             (if (sk-multi-recipe-p recipe)
			             (mapcar
			              (lambda (y)
			                (destructuring-bind (phase source &rest recipe) y
					          (make-rule (keywordicate phase '- (string-upcase target)) source recipe)))
			               recipe)
			             (make-rule target source recipe))))
		         (coerce rules 'list)))
	           '(vector rule))))          
      (unless *keep-ast* (setf (ast self) nil))
      (setf (id self) (sxhash (cons (name self) (version self))))
      self)))

;; obj -> ast
(defmethod build ((self skel-project) &key (nullp nil) (exclude '(ast id)))
  (setf (ast self)
        (unwrap-object self
                       :slots t
                       :methods nil
                       :nullp nullp
                       :exclude (append *print-slot-exclude* exclude)))
  self)

;; file -> ast
(defmethod read-ast ((self skel-project) path)
  (wrap self (file-read-forms path))
  (setf (path self) (ensure-absolute-pathname path *default-pathname-defaults*))
  self)

;; ast -> file
(defmethod write-ast :around ((self skel-project) path
			                  &key (nullp nil) (comment t) (pretty t)
			                       (if-exists :error))
  (build self :nullp nullp)
  (with-open-file (out path
                       :direction :output
                       :if-exists if-exists
                       :if-does-not-exist :create)
    (when comment 
      (princ
       (make-source-header-comment
        (name self)
        :cchar #\;
        :timestamp t
        :description (description self)
        :opts '("mode:skel;"))
       out))
    (prog1 (call-next-method self out :pretty pretty :if-exists :append)
      (unless *keep-ast* (setf (ast self) nil)))))

(defmethod wrap ((self skel-project) (config skel-user-config))
  (with-slots (vc store stash license author) (debug! config) ;; log-level, custom, fmt
    (setf (vc self) vc)
    (setf (stash self) stash)
    (setf (store self) store)
    (setf (license self) license)
    (setf (author self) author)))

(defmethod project-find ((item rule) (self skel-project) &key)
  (find (name item)
	    (rules self) 
        :test 'string-equal
        :key 'name))

(defmethod project-find ((item t) (self skel-project) &key)
  (find (string-upcase item) (rules self) :test 'string-equal :key #'sink))

(defmethod project-find ((name string) (self project-config) &key)
  (find name (scripts self) :test 'equal :key #'name))

(defmethod call ((self skel-project) (arg (eql :compile)))
  (loop for c across (components self)
	    collect (project-compile self)))

(defmethod call ((self skel-project) (arg (eql :build)))
  (loop for c across (components self)
	    collect (build self)))

(defmethod call ((self skel-project) (arg (eql :load)))
  (loop for c across (components self)
	    collect (project-load self)))

(defmethod call ((self skel-project) (arg (eql :clean)))
  (if-let ((x (project-find arg self)))
    (make self x)
    (funcall skel/core::*default-clean-function* self)))

;; (defmethod build ((self skel-project) &key)
;;   (loop for c across (components self)
;; 	    collect (build c)))

(defmethod project-compile ((self skel-project) &key)
  (loop for c across (components self)
	    collect (project-compile c)))

(defmethod project-load ((self skel-project) &key)
  (loop for c across (components self)
	    collect (project-load c)))
