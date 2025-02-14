;;; skel/core/obj.lisp --- Skel Objects

;; SKEL classes and methods

;;; Code:
(in-package :skel/core/obj)

(defclass skel (id)
  ()
  (:documentation "Base class for skeleton objects."))

(declaim (inline sk-object-name sk-slot-name))
(defun sk-class-name (self &optional downcase)
  (let* ((class-name (string (class-name (class-of self))))
         (match (search "SK-" class-name :test 'equal :start1 0 :end1 3))
         (ret (if match
                  (subseq class-name 3)
                  class-name)))
    (if downcase
        (string-downcase ret)
        ret)))

(defun sk-slot-name (self &optional downcase) 
  (keywordicate (sk-class-name self downcase)))

(defmethod sk-new ((self t) &rest initargs)
  (apply #'make-instance self initargs))

(defmethod print-object ((self skel) stream)
  (print-unreadable-object (self stream)
    (format stream "~A :ID ~A" (sk-class-name self t) (format-sxhash (id self)))))

(defmethod initialize-instance :around ((self skel) &rest initargs &key &allow-other-keys)
  ;; TODO 2023-09-10: make fast 
  (unless (getf initargs :id)
    (setf (id self) (sxhash self)))
  (when (next-method-p)
    (call-next-method)))

;; TODO 2023-09-11: research other hashing strategies - maybe use the
;; sxhash as a nonce for UUID
;; note that the sk-meta class does not inherit from skel or ast.
;;; Meta
(defclass sk-meta ()
  ((name :initarg :name :initform nil :type (or null string) :accessor name)
   (path :initarg :path :initform nil :type (or null pathname) :accessor path)
   (author :initform "" :initarg :author :type contact-designator :accessor sk-author)
   (version :initform "" :initarg :version :type string :accessor sk-version)
   (tags :initform nil :initarg :tags :accessor sk-tags)
   (description :initarg :description :initform nil :type (or null string) :accessor sk-description)
   (license :initarg :license :type license-designator :accessor sk-license))
  (:documentation "Skel Meta class."))

(defmethod print-object ((self sk-meta) stream)
  (print-unreadable-object (self stream)
    (format stream "~A ~A :path ~A" (sk-class-name self t) (name self) (path self))
    ;; (unless (sequence:emptyp (sk-version self))
    ;;   (format stream " :version ~A" (sk-version self)))
    (format stream " :id ~A" (format-sxhash (id self)))))

(defun sk-init (class &rest initargs)
  (apply #'make-instance class initargs))

(defmacro sk-init-dir (class &rest initargs)
  `(let ((self (sk-init ',class ,@initargs)))
     (unless (getf ',initargs :path)
       (setf (path self) (sb-posix:getcwd)))
     self))

(defmacro sk-init-file (class &rest initargs)
  `(let ((self (sk-init ',class ,@initargs)))
     (unless (getf ',initargs :path)
       (setf (path self) *default-skelfile*))
     self))

;;; Component

;; SK-COMPONENTs are similar in nature to ASDF/COMPONENT:COMPONENT objects but
;; much more lightweight. We use this class with the assumption that whatever
;; it's wrapping is contained within another SKEL object, such as in the
;; :COMPONENTS slots of SK-PROJECTs.

;; Container objects such as SK-PROJECT are NOT subclasses of SK-COMPONENT,
;; unlike in ASDF where systems are subclasses of components.

(defclass sk-component (skel)
  ((parent :initarg :parent :accessor sk-parent)))

;;; Module

;; Again just like ASDF, we define a SK-MOD class which subclasses
;; SK-COMPONENT. The SK-MOD class is used for components which have
;; sub-components themselves.

(defclass sk-mod (sk-component sk-meta)
  ((components :initarg :components :accessor sk-components)))

(defun make-sk-mod (form)
  "Make a new SK-MOD."
  (if (listp form)
      (apply #'make-instance 'sk-mod
             (let ((name (pop form))
                   (components 
                     (mapcar 
                      (lambda (f)
                        (sk-load-component (car f) (cdr f)))
                      form)))
               `(:name ,name :components ,components)))
      (make-instance 'sk-mod :name form :components nil)))
  
(defmethod sk-new ((self (eql :mod)) &key form path)
  (let ((mod (make-sk-mod form)))
    (when path (setf (path mod) path))
    mod))

(defmethod sk-load-component ((kind (eql :mod)) (form t) &optional (path *default-pathname-defaults*))
  (sk-new kind :form form :path path))

(defmethod sk-compile ((self sk-mod) &key)
  (dolist (c (sk-components self))
    (sk-compile c)))

(defmethod sk-build ((self sk-mod) &key)
  (dolist (c (sk-components self))
    (sk-build c)))

;;; Script

;; Scripts are always assumed to point to an executable file. They can be ran
;; directly with SK-RUN.

(defclass sk-script (sk-component sk-meta ast)
  ((kind :initform nil :initarg :kind :type (or null script-designator) :accessor sk-kind)))

(defmethod sk-new ((self (eql :script)) &key form path)
  (let ((script (make-sk-script form)))
    (setf (path script) path)
    script))

(defmethod sk-load-component ((kind (eql :script)) (form t) &optional (path *default-pathname-defaults*))
  (sk-new kind :form form :path path))

(defmethod write-ast ((self sk-script) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(path self)) :stream stream :pretty pretty :case case :readably t :array t :escape t))

(defun make-sk-script (script)
  "Make a new SK-SCRIPT."
  (apply #'make-instance 'sk-script
         (if (listp script)
             (let ((kind (first script))
                   (path (second script)))
               (list :path path
                     :name (pathname-name path)
                     :kind kind))
             (list :path script
                   :name (pathname-name script)
                   :kind (when-let ((ext (pathname-type script)))
                           (keywordicate ext))))))

(defmethod sk-run ((self sk-script))
  (sb-ext:run-program (path self) nil :output t))

(defmethod sk-write ((self sk-script) stream)
  (with-slots (path) self
    (write-string path)))

(defmethod print-object ((self sk-script) stream)
  (print-unreadable-object (self stream)
    (format stream ":~A ~A" (sk-kind self) (name self))))

;;; Config
(defconfig sk-config (skel ast) 
  ((vc :initform *default-vc-kind* :initarg :vc :type (or vc-repo vc-designator) :accessor sk-vc)
   (store :initform skel-store :initarg :store :type pathname :accessor sk-store)
   (stash :initform skel-stash :initarg :stash :type pathname :accessor sk-stash)
   (cache :initform skel-cache :initarg :cache :type pathname :accessor sk-cache)
   (registry :initform skel-registry :initarg :registry :type pathname :accessor sk-registry)
   (scripts :initform nil :initarg :scripts :type (or pathname list (vector pathname)) :accessor sk-scripts)
   (license :initform nil :initarg :license :type license-designator :accessor sk-license)
   (log-level :initform *log-level* :initarg :log-level :type log-level-designator)
   (fmt :initform :pretty :initarg :fmt :type symbol)
   (auto-insert :initform nil :initarg :auto-insert :type form))
  (:documentation "Root configuration class for the SKEL system. This class doesn't need to be exposed externally, but specifies all shared fields of SK-*-CONFIG types."))

(defmethod sk-new ((self (eql :config)) &rest args &key (type :user))
  (setf self
        (case type
          (:user 'sk-user-config)
          (:system 'sk-system-config)
          (t 'sk-config)))
  (apply #'sk-new self args))

(defmethod make-config ((self (eql :skel)) &rest args)
  (apply 'make-instance 'sk-config args))

(declaim (inline bound-string-p sk-dir))
(defun bound-string-p (o s) (and (slot-boundp o s) (stringp (slot-value o s))))
(defun sk-dir (o)
  (let ((str (directory-namestring (path o))))
    (if (sb-sequence:emptyp str)
        *default-pathname-defaults*
        (pathname str))))

(defmethod load-ast ((self sk-config))
  ;; internal ast is never tagged
  (with-slots (ast) self
    (if (formp ast)
        ;; ast is valid, modify object, set ast nil
        (progn
          (sb-int:doplist (k v) ast
            (when-let ((s (find-sk-symbol k)))
              (setf (slot-value self s) v))) ;; needs to be the correct package
          (when (bound-string-p self 'stash) (setf (sk-stash self) (merge-pathnames (sk-stash self) (sk-dir self))))
          (when (bound-string-p self 'store) (setf (sk-store self) (merge-pathnames (sk-store self) (sk-dir self))))
          (when (bound-string-p self 'cache) (setf (sk-cache self) (merge-pathnames (sk-cache self) (sk-dir self))))
          (when (bound-string-p self 'registry) (setf (sk-registry self) (merge-pathnames (sk-registry self) (sk-dir self))))
          ;; SCRIPTS
          (if (bound-string-p self 'scripts)
              (if-let* ((path (probe-file (pathname (the simple-string (sk-scripts self))))))
                       (setf (sk-scripts self)
                             (if (directory-path-p path)
                                 (find-files path)
                                 (list path)))
                       (warn! (format nil "ignoring missing scripts directory: ~A" (sk-scripts self)))))
          (unless *keep-ast* (setf (ast self) nil))
          self)
        ;; invalid ast, signal error
        (invalid-skel-ast ast))))

(defmethod build-ast ((self sk-config) &key (nullp nil) (exclude '(ast id author version user)))
  (setf (ast self)
        (unwrap-object self
                       :slots t
                       :methods nil
                       :nullp nullp
                       :exclude exclude)))

(defmethod sk-write-file ((self sk-config) 
                          &key (path *default-skelfile*) 
                               nullp
                               comment
                               (fmt :canonical)
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

(defmethod write-ast ((self sk-config) stream &key (pretty t) (case :downcase) (fmt :pretty))
  (ecase fmt
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
                        (write-ast v stream :fmt fmt)
                        (write v :stream stream :pretty pretty :case case :readably t :array t :escape t))
                    (write-char #\newline st)))
         (skel-io-error)))
    (:canonical (write (ast self) :stream stream :pretty pretty :case case :readably t :array t :escape t))))

(defclass sk-system-config (sk-config sk-meta) ())

(defun default-sk-system-config ()
  (make-instance 'sk-system-config))

(defclass sk-user-config (sk-config sk-meta)
  ((user :initarg :user :type string :accessor sk-user :initform *user*)
   (name :initarg :name :type string :accessor name)
   (email :initarg :email :type string :accessor sk-email))
  (:documentation "User configuration object, typically written to ~/.skelrc."))

(defun default-sk-user-config () (make-instance 'sk-user-config))

(declaim (type (or sk-user-config null) *skel-user-config*))
(declaim (type (or sk-system-config null) *skel-system-config*))
(defvar *skel-user-config* nil)
(defvar *skel-system-config* nil)

;;; Rule
(defstruct (sk-rule (:constructor %make-sk-rule (target source recipe)))
"Maps a SOURCE to a corresponding TARGET
via the special form stored in RECIPE."
  (target "" :type string)
  (source nil :type list)
  (recipe nil :type list))

(declaim (inline make-sk-rule))
(defun make-sk-rule (target &optional source recipe)
  (%make-sk-rule 
   (etypecase target 
     (string target)
     (symbol (string-downcase target)))
   source
   recipe))

(defmethod sk-new ((self (eql :rule)) &rest args)
  (declare (ignore self))
  (apply #'sk-new 'sk-rule args))

(defmethod id ((self sk-rule))
  (sxhash (list (sk-rule-target self) (sk-rule-source self))))

(defmethod write-ast ((self sk-rule) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(sk-rule-target self) ,(sk-rule-source self) ,@(sk-rule-recipe self)) :stream stream :pretty pretty :case case :readably t :array t :escape t))

(defmethod print-object ((self sk-rule) stream)
  (print-unreadable-object (self stream)
    (format stream "~A ~A" (sk-class-name self t) (sk-rule-target self))
    (when-let ((source (sk-rule-source self)))
      (format stream " ~A" source))))

;; Note that SK-RUN directly on a rule currently does NOT touch the sources.
(defmethod sk-run ((self sk-rule))
  (with-slots (recipe) self
    (mapcar (lambda (x)
              (etypecase x
                ((or symbol function) (funcall x :output t))
                (t (eval x))))
            recipe)))

(defmethod sk-write ((self sk-rule) stream)
  (with-slots (target source recipe) self
    (write-string (sk-rule-target target) stream) ;; target isn't typep SK-OBJECT
    (write (sk-rule-source self) :stream stream)
    (write (sk-rule-recipe self) :stream stream)))

(defun sk-make (obj &rest rules)
  (if rules
      (mapc
       (lambda (r) 
         (when-let ((rule (sk-find r obj)))
           (sk-run-with-sources obj rule)))
       rules)
      (unless (sequence:emptyp (sk-rules obj))
        (let ((rule (aref (sk-rules obj) 0)))
          (if (sk-rule-source rule)
              (sk-make obj rule)
              (sk-run rule))))))

(defun sk-run-with-sources (obj rule)
  (declare (sk-rule rule))
  (when-let ((sources (and rule (sk-rule-source rule))))
    (mapcar
     (lambda (src)
       (if-let* ((sr (sk-find src obj)))
                ;; TODO: check if we need to rerun sources
                (sk-make obj sr)
                (error "unhandled source: ~A for rule ~A" src rule)))
     sources))
  (sk-run rule))

;;; Project
(defclass sk-project (skel ast sk-meta)
  ((name :initarg :name :initform "" :type string :accessor name)
   (vc :initarg :vc :initform (vc-init *default-skel-vc-kind*) 
       :type vc-repo :accessor sk-vc)
   (src :initarg :src :type pathname :accessor sk-src)
   (stash :initarg :stash :accessor sk-stash :type pathname)
   (store :initarg :store :accessor sk-store :type pathname)
   (components :initform #() :initarg :components :accessor sk-components :type (vector sk-component))
   (bind :initarg :bind :initform nil :accessor sk-bind :type list)
   (phases :initarg :phases
           :initform (make-hash-table)
           :accessor sk-phases
           :type hash-table)
   (rules :initarg :rules
          :initform (make-array 0 :element-type 'sk-rule :adjustable t)
          :accessor sk-rules
          :type (vector sk-rule))
   (include :initarg :include
            :initform (make-array 0 :element-type 'pathname :adjustable t)
            :accessor sk-include
            :type (vector pathname))))

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
    ;; (:cmd
    ;;  ;; process the remainder as spec+defcmd-args+body
    ;;  )
    ;; (:opt
    ;;  ;; process the remainder as spec+defcmd-args+body
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
             (sym (substitute #\_ #\- (string sym))))
         (setf (uiop:getenv sym) val)
         (log:trace! "env: ~A=~A~%" sym val))))))

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
                                           (lambda (i) (load-ast
                                                        (sk-read-file
                                                         (make-instance 'sk-project)
                                                         i)))
                                           include)))
            ;; COMPONENTS
            (when (slot-boundp self 'components)
              (setf (sk-components self) (map 'vector
                                              (lambda (c)
                                                (sk-load-component 
                                                 (car c) 
                                                 (let ((val (cadr c)))
                                                   (if (listp val) val (pathname val)))
                                                 *default-pathname-defaults*))
                                              (sk-components self)))))
          ;; ;; ENV
          ;; ;; TODO
          ;; (when-let ((env (sk-env self)))
          ;;   (setf (sk-env self) (mapcar
          ;;                        (lambda (e)
          ;;                          (etypecase e
          ;;                            (symbol (cons
          ;;                                     (sb-int:keywordicate e)
          ;;                                     (sb-posix:getenv (format nil "~a" (symbol-name e)))))
          ;;                            (string (cons
          ;;                                     (sb-int:keywordicate e)
          ;;                                     (sb-posix:getenv (string-upcase e))))
          ;;                            (list
          ;;                             (cons (sb-int:keywordicate (car e)) (cadr e)))))
          ;;                        env)))
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
