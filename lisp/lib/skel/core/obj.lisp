;;; skel/core/obj.lisp --- Skel Objects

;;; Code:
(in-package :skel/core/obj)

(defclass skel (id)
  ()
  (:documentation "Base class for skeleton objects. Inherits from `sxp'."))

(defmethod sk-new ((self t) &rest initargs)
  (apply #'make-instance self initargs))

(defmethod print-object ((self skel) stream)
  (print-unreadable-object (self stream :type t)
    (format stream ":ID ~A" (format-sxhash (id self)))))

(defmethod initialize-instance :around ((self skel) &rest initargs &key &allow-other-keys)
  ;; TODO 2023-09-10: make fast 
  (unless (getf initargs :id)
    (setf (id self) (sxhash self)))
  (when (next-method-p)
    (call-next-method)))

;; TODO 2023-09-11: research other hashing strategies - maybe use the
;; sxhash as a nonce for UUID
;; note that the sk-meta class does not inherit from skel or sxp.
;;; Meta
(defclass sk-meta ()
  ((name :initarg :name :initform nil :type (or null string) :accessor sk-name)
   (path :initarg :path :initform nil :type (or null pathname) :accessor sk-path)
   (author :initform "" :initarg :author :type contact-designator :accessor sk-author)
   (version :initform "" :initarg :version :type string :accessor sk-version)
   (tags :initform nil :initarg :tags :accessor sk-tags)
   (description :initarg :description :initform nil :type (or null string) :accessor sk-description)
   (license :initarg :license :type license-designator :accessor sk-license))
  (:documentation "Skel Meta class."))

(defun sk-init (class &rest initargs)
  (apply #'make-instance class initargs))

(defmacro sk-init-dir (class &rest initargs)
  `(let ((self (sk-init ',class ,@initargs)))
     (unless (getf ',initargs :path)
       (setf (sk-path self) (sb-posix:getcwd)))
     self))

(defmacro sk-init-file (class &rest initargs)
  `(let ((self (sk-init ',class ,@initargs)))
     (unless (getf ',initargs :path)
       (setf (sk-path self) *default-skelfile*))
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

;; Again just like ASDF, we define a SK-MODULE class which subclasses
;; SK-COMPONENT. The SK-MODULE class is used for components which have
;; sub-components themselves.

(defclass sk-module (sk-component sk-meta)
  ((components :initarg :components :accessor sk-components)))

;;; Script
(defclass sk-script (sk-component sk-meta sxp)
  ((kind :initform nil :initarg :kind :type (or null script-designator) :accessor sk-kind)))

(defmethod write-sxp-stream ((self sk-script) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(sk-path self)) :stream stream :pretty pretty :case case :readably t :array t :escape t))

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
  (sb-ext:run-program (sk-path self) nil :output t))

(defmethod sk-write ((self sk-script) stream)
  (with-slots (path) self
    (write-string path)))

(defmethod print-object ((self sk-script) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A :~A ~A" (format-sxhash (id self)) (sk-kind self) (sk-name self))))

;;; Snippet
(defstruct sk-snippet
  (name "" :type string)
  (form "" :type form))

(defmethod sk-new ((self (eql :snippet)) &key name form)
  (declare (ignore self))
  (make-sk-snippet :name name :form form))

;;; Abbrev
(defstruct sk-abbrev
  (match nil :type form) 
  (expansion nil :type form))

(defmethod sk-new ((self (eql :abbrev)) &key match expansion)
  (declare (ignore self))
  (make-sk-abbrev :match match :expansion expansion))

;;; Config
(defclass sk-config (skel sxp) 
  ((vc :initform *default-skel-vc-kind* :initarg :vc :type (or vc-designator sk-vc-meta) :accessor sk-vc)
   (store :initform *skel-store* :initarg :store :type pathname :accessor sk-store)
   (stash :initform *skel-stash* :initarg :stash :type pathname :accessor sk-stash)
   (cache :initform *skel-cache* :initarg :cache :type pathname :accessor sk-cache)
   (registry :initform *skel-registry* :initarg :registry :type pathname :accessor sk-registry)
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

(declaim (inline bound-string-p sk-dir))
(defun bound-string-p (o s) (and (slot-boundp o s) (stringp (slot-value o s))))
(defun sk-dir (o)
  (let ((str (directory-namestring (sk-path o))))
    (unless (sb-sequence:emptyp str)
      str)))

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
          (when (bound-string-p self 'scripts) (setf (sk-scripts self)
                                                     ;; TODO 2023-10-14: convert into list of script names
                                                     (merge-pathnames (sk-scripts self) (sk-dir self))))
          (unless *keep-ast* (setf (ast self) nil))
          self)
        ;; invalid ast, signal error
        (invalid-skel-ast ast))))

(defmethod build-ast ((self sk-config) &key (nullp nil) (exclude '(ast id)))
  (setf (ast self) 
        (unwrap-object self
                       :slots t
                       :methods nil
                       :nullp nullp
                       :exclude exclude)))

(defmethod sk-write-file ((self sk-config) 
                          &key (path *default-skelfile*) 
                               (nullp nil) 
                               (header t) 
                               (fmt :canonical)
                               (if-exists :error))
  (build-ast self :nullp nullp)
  (prog1 
      (with-open-file (out path
                           :direction :output
                           :if-exists if-exists
                           :if-does-not-exist :create)
        (when header (princ
                      (make-source-header-comment
                       (sk-name self)
                       :cchar #\;
                       :timestamp t
                       :description (sk-description self)
                       :opts '("mode:skel;"))
                      out))
        (write-sxp-stream self out :fmt fmt))
    (unless *keep-ast* (setf (ast self) nil))))

(defmethod write-sxp-stream ((self sk-config) stream &key (pretty t) (case :downcase) (fmt :pretty))
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
                        (write-sxp-stream v stream :fmt fmt)
                        (write v :stream stream :pretty pretty :case case :readably t :array t :escape t))
                    (write-char #\newline st)))
         (skel-io-error)))
    (t (write (ast self) :stream stream :pretty pretty :case case :readably t :array t :escape t))))

(defclass sk-system-config (sk-config sk-meta) ())

(defun default-sk-system-config ()
  (make-instance 'sk-system-config))

(defclass sk-user-config (sk-config sk-meta)
  ((user :initarg :user :type string :accessor sk-user)
   (name :initarg :name :type string :accessor sk-name)
   (email :initarg :email :type string :accessor sk-email))
  (:documentation "User configuration object, typically written to ~/.skelrc."))

(defun default-sk-user-config () (make-instance 'sk-user-config))

(declaim (type (or sk-user-config null) *skel-user-config*))
(declaim (type (or sk-system-config null) *skel-system-config*))
(defvar *skel-user-config* nil)
(defvar *skel-system-config* nil)

;;; Command
(defclass sk-command (skel)
  ((body :initform nil :initarg :body :type (or form function) :accessor sk-body)))

(defmethod sk-new ((self (eql :command)) &key body)
  (make-instance 'sk-command :body body))

(defmethod sk-write ((self sk-command) stream)
  (if (stringp (sk-body self)) (format stream "~A" (sk-body self))))

(defmethod sk-write-string ((self sk-command))
  (with-output-to-string (s)
    (sk-write self s)))

(defmethod sk-writeln ((self sk-command) stream) 
  (sk-write self stream)
  (format stream "~%"))

(defmethod write-sxp-stream ((self sk-command) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,@(sk-body self)) :stream stream :pretty pretty :case case :readably t :array t :escape t))

(defmethod sk-run ((self sk-command))
  (mapcar (lambda (x) (funcall x :output t))
          (sk-body self)))

;;; Rule
(defclass sk-rule (skel)
  ;; RESEARCH 2024-05-11: consider more options for extending target slot
  ((target :initarg :target :type string :accessor sk-rule-target)
   (source :initform nil :initarg :source :type list :accessor sk-rule-source)
   (recipe :initform (make-instance 'sk-command) :initarg :recipe :type sk-command :accessor sk-rule-recipe))
  (:documentation "Skel rules. Maps a SOURCE to a corresponding TARGET
via the special form stored in RECIPE."))

(defmethod sk-new ((self (eql :rule)) &rest args)
  (declare (ignore self))
  (apply #'sk-new 'sk-rule args))

(defmethod write-sxp-stream ((self sk-rule) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(sk-rule-target self) ,(sk-rule-source self) ,@(sk-body (sk-rule-recipe self))) :stream stream :pretty pretty :case case :readably t :array t :escape t))

(defun make-sk-rule (target source recipe)
  "Make a new SK-RULE."
  (let ((r (make-instance 'sk-command :body recipe)))
    (make-instance 'sk-rule :target (format nil "~(~a~)" target) :source source :recipe r)))

(defmethod print-object ((self sk-rule) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (sk-rule-target self))
    (when-let ((source (sk-rule-source self)))
      (format stream " :source ~A" source))))

;; Note that SK-RUN directly on a rule currently does NOT touch the sources.
(defmethod sk-run ((self sk-rule))
  (with-slots (recipe) self
    (mapcar (lambda (x)
              (etypecase x
                ((or symbol function) (funcall x :output t))
                (t (eval x))))
            (sk-body recipe))))

(defmethod sk-write ((self sk-rule) stream)
  (with-slots (target source recipe) self
    (write-string target) ;; target isn't typep SK-OBJECT
    (sk-write-string source)
    (sk-write-string recipe)))

(defun sk-make (obj &rest rules)
  (if rules
      (mapc
       (lambda (rule)
         (when-let ((sources (sk-rule-source rule)))
           (mapcar
            (lambda (src)
              (if-let* ((sr (sk-find-rule src obj)))
                 ;; check if we need to rerun sources
                (sk-make obj sr)
                (warn! "unhandled source:" src "for rule:" rule)))
            sources))
         (sk-run rule))
       rules)
      (unless (sequence:emptyp (sk-rules obj))
        (let ((rule (aref (sk-rules obj) 0)))
          (if (sk-rule-source rule)
              (sk-make obj rule)
              (sk-run rule))))))

;;; Version Control
(defstruct sk-vc-remote-meta
  (name :default :type keyword)
  (path nil :type (or symbol string)))

(defmethod write-sxp-stream ((self sk-vc-remote-meta) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(sk-vc-remote-meta-name self) ,(sk-vc-remote-meta-path self)) :stream stream :pretty pretty :case case :readably t :array t :escape t))

(defstruct (sk-vc-meta (:constructor make-sk-vc-meta (kind &optional remotes)))
  (kind *default-skel-vc-kind* :type vc-designator)
  (remotes nil :type (or string list)))

(defmethod write-sxp-stream ((self sk-vc-meta) stream &key (pretty t) (case :downcase) (fmt :pretty))
  (if (= 0 (length (sk-vc-meta-remotes self)))
      (write (sk-vc-meta-kind self) :stream stream :pretty pretty :case case :readably t :array t :escape t)
      (progn
        (format stream "(")
        (write (sk-vc-meta-kind self) :stream stream :pretty pretty :case case :readably t :array t :escape t)      
        (format stream " ")
        (loop for x in (sk-vc-meta-remotes self)
              do 
                 (write-sxp-stream x stream :pretty pretty :case case :fmt fmt))
        (format stream ")"))))

(defmethod print-object ((self sk-vc-meta) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~S" (sk-vc-meta-kind self))
    (when-let ((remotes (sk-vc-meta-remotes self)))
      (format stream " ~A" remotes))))
;;; Project
(defclass sk-project (skel sxp sk-meta)
  ((name :initarg :name :initform "" :type string)
   (src :initarg :src :type pathname :accessor sk-src)
   (vc :initarg :vc :initform (make-sk-vc-meta *default-skel-vc-kind*) :type sk-vc-meta :accessor sk-vc)
   (rules :initarg :rules
          :initform (make-array 0 :element-type 'sk-rule :adjustable t)
          :accessor sk-rules
          :type (vector sk-rule))
   (components :initform #() :initarg :components :accessor sk-components :type (vector (cons keyword pathname)))
   (bind :initarg :bind :initform nil :accessor sk-bind :type list)
   (env :initarg :env :initform nil :accessor sk-env :type list)
   (scripts :initarg :scripts
            :initform (make-array 0 :element-type 'sk-script :adjustable t)
            :accessor sk-scripts
            :type (vector sk-script))
   (stash :initarg :stash :accessor sk-stash :type pathname)
   (store :initarg :store :accessor sk-store :type pathname)
   (include :initarg :include
            :initform (make-array 0 :element-type 'pathname :adjustable t)
            :accessor sk-include
            :type (vector pathname))))

(defmethod print-object ((self sk-project) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A [c=~A;i=~A;r=~A;s=~A] :id ~A"
            (sk-name self)
            (length (sk-components self))
            (length (sk-include self))
            (length (sk-rules self))
            (length (sk-scripts self))
            (format-sxhash (id self)))))

(defmethod sk-new ((self (eql :project)) &rest args)
  (declare (ignore self))
  (apply #'sk-new 'sk-project args))

(defun find-sk-symbol (s)
  (find-symbol* (symbol-name s) :skel/core/obj t))

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
              (setf (sk-src self) (probe-file (sk-src self)))
              (setf (sk-src self) (or (sk-dir self) *default-pathname-defaults*)))
          (setq *skel-path* (sk-src self))
          (when (bound-string-p self 'stash) (setf (sk-stash self) (pathname (the simple-string (sk-stash self)))))
          (when (bound-string-p self 'store) (setf (sk-store self) (pathname (the simple-string (sk-store self)))))
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
                                              (sk-load-component (car c) (pathname (cadr c)) (namestring *skel-path*)))
                                            (sk-components self))))
          ;; SCRIPTS
          (if (bound-string-p self 'scripts)
              (if-let* ((path (probe-file (pathname (the simple-string (sk-scripts self))))))
                       (setf (sk-scripts self)
                             (if (directory-path-p path)
                                 (find-files path)
                                 (list path)))
                       (warn! (format nil "ignoring missing scripts directory: ~A" (sk-scripts self)))))
          (when-let ((scripts (sk-scripts self)))
            (setf (sk-scripts self) (map 'vector #'make-sk-script scripts)))
          ;; ENV
          ;; TODO
          (when-let ((env (sk-env self)))
            (setf (sk-env self) (mapcar
                                 (lambda (e)
                                   (etypecase e
                                     (symbol (cons
                                              (sb-int:keywordicate e)
                                              (sb-posix:getenv (format nil "~a" (symbol-name e)))))
                                     (string (cons
                                              (sb-int:keywordicate e)
                                              (sb-posix:getenv (string-upcase e))))
                                     (list
                                      (cons (sb-int:keywordicate (car e)) (cadr e)))))
                                 env)))
          ;; RULES
          (when-let ((rules (sk-rules self)))
            (setf (sk-rules self) (map 'vector
                                       (lambda (x)
                                         (destructuring-bind (target source &rest recipe) x
                                           (make-sk-rule target source recipe)))
                                       rules)))
          ;; VC
          (when-let ((vc (sk-vc self)))
            (etypecase vc
              ((or sk-vc-meta null) nil)
              (vc-designator (setf (sk-vc self) (make-sk-vc-meta vc)))
              (list (setf (sk-vc self) (apply #'make-sk-vc-meta vc)))))
          
          (unless *keep-ast* (setf (ast self) nil))
          (setf (id self) (sxhash (cons (sk-name self) (sk-version self))))
          self)
        ;; invalid ast, signal error
        (invalid-skel-ast ast))))

;; obj -> ast
(defmethod build-ast ((self sk-project) &key (nullp nil) (exclude '(ast id)))
  (setf (ast self)
        (unwrap-object self
                       :slots t
                       :methods nil
                       :nullp nullp
                       :exclude exclude)))

;; TODO 2023-09-26: This belongs in sxp
(defmethod write-sxp-stream ((self sk-project) stream &key (pretty t) (case :downcase) (fmt :pretty))
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
		        (write-sxp-stream v stream :pretty pretty :case case)
		        (write v :stream stream :pretty pretty :case case :readably t :array t :escape t))
		    (write-char #\newline st)))
	 (skel-io-error)))
    (t (write (ast self) :stream stream :pretty pretty :case case :readably t :array t :escape t))))

;; file -> ast
(defmethod sk-read-file ((self sk-project) path)
  (wrap self (file-read-forms path))
  (setf (sk-path self) (ensure-absolute-pathname path *default-pathname-defaults*))
  ;; TODO 2024-04-18: make generic
  self)

;; ast -> file
(defmethod sk-write-file ((self sk-project) 
			  &key 
                          (path *default-skelfile*) (nullp nil) (header t) (fmt :canonical)
                          (if-exists :error))
  (build-ast self :nullp nullp)
  (prog1 
      (with-open-file (out path
                           :direction :output
                           :if-exists if-exists
                           :if-does-not-exist :create)
        (when header (princ
                      (make-source-header-comment
                       (sk-name self)
                       :cchar #\;
                       :timestamp t
                       :description (sk-description self)
                       :opts '("mode:skel;"))
                      out))
        (write-sxp-stream self out :fmt fmt))
    (unless *keep-ast* (setf (ast self) nil))))

(defmethod sk-install-user-config ((self sk-project) (cfg sk-user-config))
  (with-slots (vc store stash license author) (debug! cfg) ;; log-level, custom, fmt
    (setf (sk-vc self) vc)
    (setf (sk-stash self) stash)
    (setf (sk-store self) store)
    (setf (sk-license self) license)
    (setf (sk-author self) author)))

(defmethod sk-find-rule (name self)
  (find (string-upcase name) (sk-rules self) :test 'equalp :key #'sk-rule-target))

(defmethod sk-find-script ((name string) (self skel) &key)
  (find name (sk-scripts self) :test 'equal :key #'sk-name))

(defmethod sk-call ((self sk-project) (arg sk-rule))
  (sk-make self arg))

(defmethod sk-call ((self sk-project) (arg t))
  (sk-make self (sk-find-rule arg self)))

(defmethod sk-call* ((self sk-project) &rest args)
  (mapcar (lambda (arg) (sk-call self arg)) args))
