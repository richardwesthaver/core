;;; Objects
(in-package :skel/core)

;;; Vars
(declaim (type vc-designator *default-skel-vc-kind*))
(deftype vc-designator () '(member :hg :git list))

;; ref: https://spdx.org/licenses/
(deftype license-designator () '(or string pathname (member :mpl2 :wtfpl :lgpg :llgpl :gpl :mit :mit0)))

(defparameter *default-skel-vc-kind* :hg)
(defparameter *default-skel-license-kind* :mpl2)
(declaim (type sk-project *skel-project*))
(defvar *skel-project*)
;; TODO (defvar *skelfile-boundary* nil "Set an upper bounds on how
;; many times and how far to walk an arbitrary file directory.")

(declaim (type string *default-skel-user* *default-skelfile* *default-skel-extension*))
(defparameter *default-skel-user* (uid-username (unix-getuid)))
(defparameter *default-skelfile* "skelfile")
(defparameter *default-skel-extension* "sk")
(defparameter *default-skelrc* ".skelrc")

(declaim (type pathname *skel-stash* *skel-store*
	       *skel-cache* *user-skelrc* *system-skelrc*))

(defparameter *skel-stash* #P"/usr/local/share/skel/stash/")

(defparameter *skel-store* #P"/usr/local/share/skel/store/")

(defparameter *skel-cache* #P"/usr/local/share/skel/cache/")

(defparameter *skel-registry* #P"/usr/local/share/skel/registry/")

(defparameter *user-skelrc* (pathname (format nil "~~/~A" *default-skelrc*)))

(defparameter *system-skelrc* (pathname "/etc/skelrc"))

(defparameter *keep-ast* nil
  "Whether to keep the :ast slot stored with an sk object, or set it to nil so
that it can be GC'd.")

;;; Objects
(defclass skel (id)
  ()
  (:documentation "Base class for skeleton objects. Inherits from `sxp'."))

(defmethod print-object ((self skel) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~S ~A" :id (format-sxhash (id self)))))

(defmethod initialize-instance :around ((self skel) &rest initargs &key &allow-other-keys)
  ;; TODO 2023-09-10: make fast 
  (unless (getf initargs :id)
    (setf (id self) (sxhash self)))
  (when (next-method-p)
    (call-next-method)))

;; TODO 2023-09-11: research other hashing strategies - maybe use the
;; sxhash as a nonce for UUID
;; note that the sk-meta class does not inherit from skel or sxp.
;;;; Meta
(defclass sk-meta ()
  ((name :initarg :name :initform nil :type (or null string) :accessor sk-name)
   (path :initarg :path :initform nil :type (or null pathname) :accessor sk-path)
   (author :initform "" :initarg :author :type string :accessor sk-author)
   (version :initform "" :initarg :version :type string :accessor sk-version)
   (tags :initform nil :initarg :tags :accessor sk-tags)
   (description :initarg :description :initform nil :type (or null string) :accessor sk-description)
   (license :initform nil :initarg :license :type :string :accessor sk-license))
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

;;;; Command
(defclass sk-command (skel)
  ((body :initform nil :initarg :body :type (or form function) :accessor sk-body)))

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

;;  HACK 2023-09-27: (defstruct sk-url) ?

;;;; Source
(defclass sk-source (skel)
  ((path :initform "" :initarg :path :type string :accessor sk-path)))

(defmethod sk-write ((self sk-source) stream)
  (if (stringp (sk-path self)) (format stream "~A" (sk-path self))))

(defmethod sk-write-string ((self sk-source))
  (with-output-to-string (s)
    (sk-write self s)))

;;;; Rule
(defclass sk-rule (skel)
  ;; RESEARCH 2024-05-11: consider more options for extending target slot
  ((target :initarg :target :type string :accessor sk-rule-target)
   (source :initform nil :initarg :source :type list :accessor sk-rule-source)
   (recipe :initform (make-instance 'sk-command) :initarg :recipe :type sk-command :accessor sk-rule-recipe))
  (:documentation "Skel rules. Maps a SOURCE to a corresponding TARGET
via the special form stored in RECIPE."))

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
    (mapcar (lambda (x) (funcall x :output t))
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
              (if-let ((sr (sk-find-rule src obj)))
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

;;;; Document
(deftype document-designator () '(member :org :txt :pdf :html :md))

;; TODO 2023-10-13: integrate organ for working with org document
;; types - mixins and such
(defclass sk-document (skel sk-meta sxp)
  ((kind :initarg :kind :type document-designator :accessor sk-kind)
   (export :initarg :export :type form :accessor sk-export
	   :documentation "document export options")
   (attach :initarg :attach :type form :accessor sk-attach
	   :documentation "document attachments"))
  (:documentation "Document object."))

(defun make-sk-document (kind path &key export attach)
  "Make a new SK-RULE."
  ;;  TODO 2024-05-10: component paths ala asdf
  (make-instance 'sk-document
    :name (pathname-name path)
    :kind (sb-int:keywordicate (string-upcase (format nil "~a" kind)))
    :path path
    :export export
    :attach attach))

(defmethod print-object ((self sk-document) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~S ~A" (sb-int:keywordicate (sk-kind self)) (sk-path self))))

(defmethod write-sxp-stream ((self sk-document) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(keywordicate (sk-kind self)) ,(sk-path self)
           ,@(when-let ((e (sk-export self))) (list :export e))
           ,@(when-let ((a (sk-attach self))) (list :attach a)))
         :stream stream
         :pretty pretty
         :case case
         :readably t
         :array t
         :escape t))

(defmethod sk-write ((self sk-document) stream)
  (write-string (keywordicate (sk-kind self)))
  (sk-write-string (sk-path self)))

;;;; Script
(deftype script-designator () '(member :bin :sh :bash :zsh :nu :lisp :python))

(defclass sk-script (skel sk-meta sxp)
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

;;;; Config
(defclass sk-config (skel sxp) 
  ((imports :initarg :imports :type list)
   (vc :initform *default-skel-vc-kind* :initarg :vc :type (or vc-designator sk-vc-meta) :accessor sk-vc)
   (store :initform *skel-store* :initarg :store :type pathname :accessor sk-store)
   (stash :initform *skel-stash* :initarg :stash :type pathname :accessor sk-stash)
   (cache :initform *skel-cache* :initarg :cache :type pathname :accessor sk-cache)
   (registry :initform *skel-registry* :initarg :registry :type pathname :accessor sk-registry)
   (scripts :initarg :scripts :type (or pathname list (vector pathname)) :accessor sk-scripts)
   (license :initarg :license :type license-designator :accessor sk-license)
   (log-level :initarg :log-level :type log-level-designator)
   (fmt :initform :pretty :initarg :fmt :type symbol)
   (alias-list :initarg :alias-list
               :type (or list vector)
	       :documentation "alist of aliases. currently used as a special cli-opt-parser by the skel binary.")
   (auto-insert :initform nil :initarg :auto-insert :type form))
  (:documentation "Root configuration class for the SKEL system. This class doesn't need to be exposed externally, but specifies all shared fields of SK-*-CONFIG types."))

(defun bound-string-p (o s) (and (slot-boundp o s) (stringp (slot-value o s))))

(defmethod load-ast ((self sk-config))
  ;; internal ast is never tagged
  (with-slots (ast) self
    (if (formp ast)
	;; ast is valid, modify object, set ast nil
	(progn
	  (sb-int:doplist (k v) ast
            (when-let ((s (find-sk-symbol k)))
	      (setf (slot-value self s) v))) ;; needs to be the correct package
	  (when (bound-string-p self 'stash) (setf (sk-stash self) (pathname (sk-stash self))))
	  (when (bound-string-p self 'store) (setf (sk-store self) (pathname (sk-store self))))
	  (when (bound-string-p self 'cache) (setf (sk-cache self) (pathname (sk-cache self))))
	  (when (bound-string-p self 'registry) (setf (sk-registry self) (pathname (sk-registry self))))
	  (when (bound-string-p self 'scripts) (setf (sk-scripts self)
					             ;; TODO 2023-10-14: convert into list of script names
					             (pathname (sk-scripts self))))
	  (unless *keep-ast* (setf (ast self) nil))
	  self)
	;; invalid ast, signal error
	(skel-syntax-error ast))))

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

(defclass sk-system-config (sk-config sk-meta) ())

(defun default-sk-system-config ()
  (make-instance 'sk-system-config))

(defclass sk-user-config (sk-config sk-meta)
  ((user :initarg :user :type form :accessor sk-user)
   (name :initarg :name :type form :accessor sk-name))
  (:documentation "User configuration object, typically written to ~/.skelrc."))

(defun default-sk-user-config () (make-instance 'sk-user-config))

(declaim (type sk-user-config *skel-user-config*))
(declaim (type sk-system-config *skel-system-config*))
(defvar *skel-user-config* (default-sk-user-config))
(defvar *skel-system-config* (default-sk-system-config))

;;;; Snippet
(defstruct sk-snippet
  (name "" :type string)
  (form "" :type form))

;;;; Abbrev
(defstruct sk-abbrev
  (match nil :type form) 
  (expansion nil :type form))

;;;; Version Control
(defstruct sk-vc-remote-meta
  (name :default :type keyword)
  (path nil :type (or symbol string)))

(defmethod write-sxp-stream ((self sk-vc-remote-meta) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(sk-vc-remote-meta-name self) ,(sk-vc-remote-meta-path self)) :stream stream :pretty pretty :case case :readably t :array t :escape t))

(defstruct (sk-vc-meta (:constructor make-sk-vc-meta (kind &optional remotes)))
  (kind *default-skel-vc-kind* :type vc-designator)
  (remotes nil :type (or string list)))

(defmethod write-sxp-stream ((self sk-vc-meta) stream &key (pretty t) (case :downcase) (fmt :collapsed))
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

;;;; Project
(defclass sk-project (skel sxp sk-meta)
  ((name :initarg :name :initform "" :type string)
   (vc :initarg :vc :initform (make-sk-vc-meta *default-skel-vc-kind*) :type sk-vc-meta :accessor sk-vc)
   (rules :initarg :rules
          :initform (make-array 0 :element-type 'sk-rule :adjustable t)
          :accessor sk-rules
          :type (vector sk-rule))
   (docs :initarg :documents
         :initform (make-array 0 :element-type 'sk-document :adjustable t)
         :accessor sk-docs :type (vector sk-document))
   (components :initarg :components :initform nil :accessor sk-components :type list)
   (vars :initarg :vars :initform nil :accessor sk-vars :type list)
   (env :initarg :env :initform nil :accessor sk-env :type list)
   (scripts :initarg :scripts
            :initform (make-array 0 :element-type 'sk-script :adjustable t)
            :accessor sk-scripts
            :type (vector sk-script))
   (snippets :initarg :snippets
             :initform (make-array 0 :element-type 'sk-snippet :adjustable t)
             :accessor sk-snippets
             :type (vector sk-snippet))
   (stash :initarg :stash :accessor sk-stash :type pathname)
   (store :initarg :store :accessor sk-store :type pathname)
   (abbrevs :initarg :abbrevs
            :initform (make-array 0 :element-type 'sk-abbrev :adjustable t)
            :accessor sk-abbrevs
            :type (vector sk-abbrevs))
   (imports :initarg :imports
            :initform (make-array 0 :element-type 'pathname :adjustable t)
            :accessor sk-imports
            :type (vector pathname))))

(defun find-sk-symbol (s)
  (find-symbol* (symbol-name s) :skel/core nil))

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
          (when (bound-string-p self 'stash) (setf (sk-stash self) (pathname (the simple-string (sk-stash self)))))
          (when (bound-string-p self 'store) (setf (sk-store self) (pathname (the simple-string (sk-store self)))))
          (if (bound-string-p self 'scripts)
              (if-let ((path (probe-file (pathname (the simple-string (sk-scripts self))))))
                (setf (sk-scripts self)
                      (if (directory-path-p path)
                          (find-files path)
                          (list path)))
                (debug! (format nil "ignoring missing scripts directory: ~A" (sk-scripts self)))))
          (when-let ((docs (sk-docs self)))
            (setf (sk-docs self) (map 'vector (lambda (d) (apply #'make-sk-document d)) docs)))
          (when-let ((scripts (sk-scripts self)))
            (setf (sk-scripts self) (map 'vector #'make-sk-script scripts)))
          (when-let ((env (sk-env self)))
            (setf (sk-env self) (mapcar
                                 (lambda (e)
                                   (etypecase e
                                     (symbol (cons
                                              (sb-int:keywordicate e)
                                              (sb-posix:getenv (format nil "~a" (symbol-name e)))))
                                     (string (cons
                                              (sb-int:keywordicate e)
                                              (sb-posix:getenv (format nil "~a" (symbol-name e)))))
                                     (list
                                      (cons (sb-int:keywordicate (car e)) (cdr e)))))
                                 
                                 env)))
          (when-let ((rules (sk-rules self)))
            (setf (sk-rules self) (map 'vector
                                       (lambda (x)
                                         (destructuring-bind (target source &rest recipe) x
                                           (make-sk-rule target source recipe)))
                                       rules)))
          (when-let ((vc (sk-vc self)))
            (etypecase vc
              ((or sk-vc-meta null) nil)
              (vc-designator (setf (sk-vc self) (make-sk-vc-meta vc)))
              (list (setf (sk-vc self) (apply #'make-sk-vc-meta vc)))))
          (unless *keep-ast* (setf (ast self) nil))
          (setf (id self) (sxhash (cons (sk-name self) (sk-version self))))
          self)
        ;; invalid ast, signal error
        (skel-syntax-error ast))))

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
	 (error 'sxp-fmt-error)))
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
    (cas (sk-vc self) nil vc)
    (cas (sk-stash self) nil stash)
    (cas (sk-store self) nil store)
    (cas (sk-license self) nil license)
    (cas (sk-author self) nil author)))

(defmethod sk-find-rule (name self)
  (find (string-upcase name) (sk-rules self) :test 'equalp :key #'sk-rule-target))

(defmethod sk-find-script ((name string) (self skel) &key)
  (find name (sk-scripts self) :test 'equal :key #'sk-name))
