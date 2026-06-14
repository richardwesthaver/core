;;; project.lisp --- Project Protocols

;; The PROJECT base class.

;;; Commentary:

;; The SK-PROJECT class used to be 'the base class', but not all projects
;; necessarily fit into the SKEL protocol. A much more generic starting point
;; is needed.

;; The PROJECT class contains an unusually large number of slots for a
;; base. We want to fit as much of the common 'metadata' involved in projects
;; here so that we don't have to duplicate them elsewhere.

;; The PROJECT class always contains a dedicated AST slot containing an S-expr
;; representation of the project or NIL. This slot is used as a buffer when
;; reading or writing projects.

;;; Code:
(in-package :obj/project)

;;; Variables
(defvar *default-project-class* 'simple-project)
(defvar *default-rule-class* 'simple-rule)
(defglobal *project-table* (make-hash-table)
  "An EQL hash-table containing all registered projects.")

(defvar *project* nil "The currently active PROJECT instance.")
(defvar *project-config* nil "The currently active PROJECT-CONFIG instance.")

(defparameter *project-hook* (make-instance 'std:key-hook))

(defvar-unbound *rule* "The currently active RULE instance.")

;;; Conditions
(defcondition project-condition () 
  ((project :initform *project* :accessor error-project :initarg :project))
  (:error-class project-error (error) () (:reporter t))
  (:warning-class project-warning (simple-warning) () (:reporter t)))

;;; Protocol
(defclass project-metadata ()
  ((name :initarg :name :accessor name)
   (path :initarg :path :accessor path)
   (author :initarg :author :accessor author)
   (version :initarg :version :accessor version)
   (tags :initarg :tags :accessor tags)
   (links :initarg :links :accessor links)
   (description :initarg :description :type (or null string) :accessor description)
   (license :initarg :license :accessor license))
  (:documentation "Project Metadata contains optional slots which may be inherited by
project-like objects."))

(defmethod print-object ((self project-metadata) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A :path ~A" (name self) (path self))
    (unless (sequence:emptyp (version self))
      (format stream " :version ~A" (version self)))
    (format stream " :id ~A" (format-sxhash (id self)))))

(defclass project (id ast) ()
  (:documentation "A generic project (without metadata)."))

(defmethod print-object ((self project) stream)
  (print-unreadable-object (self stream :type t)
    (princ (name self) stream)))

(defclass simple-project (project project-metadata module) ()
  (:documentation "A PROJECT with optional metadata."))

(defgeneric project-compile (self &key path &allow-other-keys))
(defgeneric project-load (self &key &allow-other-keys))
(defgeneric project-find (what self &key &allow-other-keys))
(defgeneric project-convert (self))
(defgeneric load-project-component (kind form &key &allow-other-keys))

(defun project (name)
  "Find a registered project by NAME."
  (gethash name *project-table*))

(defun (setf project) (project name)
  "Find a registered project by NAME."
  (setf (gethash name *project-table*) project))

(defun register-project (project)
  (setf (project (name project)) project))

(defun make-project (name &rest args &key (class *default-project-class*) &allow-other-keys)
  (apply 'make-instance class :name name (remove-from-plist args :class)))

(defun project-slot (slot &key package (default :error))
  (if-let ((slot (find-symbol* (string-upcase (string slot)) package nil)))
    (if (or (null *project*) (not (slot-boundp* *project* slot)))
        ;; Not found in project, search config files instead
        (project-config-slot slot default)
        (slot-value *project* slot))
    (if (eql default :error)
        (std-error "slot is unbound in project")
        default)))

(defun search-project (query &optional (project *project*) (config *project-config*))
  "Search the current project for elements matching QUERY."
  (etypecase query
    (string (or (project-find query project)
                (project-find query config)))
    (integer (or (project-find query project :slot :id)
                 (project-find query config :slot :id)))
    (keyword (project-slot query))))

(defun project-link (key &optional (project *project*))
  (if project
    (getf key (links project))
    (project-warning "No active *PROJECT*.")))

(defun project-tag-p (tag &optional (project *project*))
  (if project
      (find tag (tags project))
      (project-warning "No active *PROJECT*.")))

(defun list-all-projects () (hash-table-values *project-table*))

;;; Config
(defconfig project-config (project-metadata ast) ()
  (:documentation "A generic project configuration."))

(defun project-config-slot (slot &optional package (default :error))
  "First check *SKEL-USER-CONFIG* for a slot value, and if a valid value
isn't found check *SKEL-SYSTEM-CONFIG*."
  (flet ((bail ()
           (if (eql default :error)
               (std-error "slot is unbound in project config")
               (return-from project-config-slot default))))
    (declare (dynamic-extent #'bail))
    (if-let ((slot (find-symbol (string-upcase (string slot)) package)))
      (if (or (null *project-config*) (not (slot-boundp* *project-config* slot)))
          (bail)
          (slot-value *project-config* slot))
      (bail))))

;;; Rules
(defkernel rule (kernel-object ast id) ()
  (:documentation "Funcallable objects with an ID and AST."))

(defaccessor name ((self rule)) (id self))

(defmethod write-ast ((self rule) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(sink self) ,(source self) ,@(ast self)) :stream stream :pretty pretty :case case :readably t :array t :escape t))

(defmethod print-object ((self rule) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (name self))))

(defmethod write-object ((self rule) stream &key)
  (write-string (name self) stream)
  (write (ast self) :stream stream))

(defkernel simple-rule (rule)
  ((source :type list :accessor source :initarg :source)
   (target :accessor rule-target :initarg :target))
  (:documentation "A 'simple' rule containing a SOURCE and TARGET, similar to GNU Makefile rules."))

(defmethod write-ast ((self simple-rule) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(rule-target self) ,(source self) ,@(ast self)) :stream stream :pretty pretty :case case :readably t :array t :escape t))

(defmethod print-object ((self simple-rule) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (rule-target self))
    (when-let ((source (source self)))
      (format stream " ~{~(~A~)~^ ~}" source))))

(defmethod write-object ((self simple-rule) stream &key)
  (write-string (rule-target self) stream) ;; target isn't typep SK-OBJECT
  (write (source self) :stream stream)
  (write (ast self) :stream stream))

(defmethod exec ((self simple-rule)) (compile-and-eval* (ast self)))
(defmethod id ((self simple-rule)) (sxhash (list (rule-target self) (source self))))
(defaccessor name ((self simple-rule)) (rule-target self))
(defaccessor sink ((self simple-rule)) (rule-target self))

(defverb rules (self) 
  (:accessor t)
  (:documentation "Return the ruleset of object SELF."))

(defkernel interactive-rule (rule command)
  ((args :initform nil :type list))
  (:documentation "Rules which support the command protocol."))

(defkernel simple-interactive-rule (interactive-rule simple-rule)
  ((args :initform nil :type list))
  (:documentation "Rules which support the command protocol."))

(definline make-rule (target &optional source recipe (class *default-rule-class*))
  (make-instance class
    :target (etypecase target 
              (string target)
              (symbol (string-downcase target)))
    :source source
    :ast
    (multiple-value-bind (form _ doc) (parse-body recipe :documentation t)
      ;; TODO 2025-02-25: figure out where to put the docstring - hash,compare,cache
      (declare (ignore _ doc))
      form)))

;;; Components
(defclass project-component (id component ast)
  ((parent :initarg :parent :accessor parent))
  (:default-initargs :ast nil))

(defmethod print-object ((self project-component) stream)
  (print-unreadable-object (self stream :type t)
    (when-let ((name (or (name self) (format-sxhash (id self)))))
      (format stream "~A" name))))

(defmethod load-ast ((self project-component))
  (let ((form (ast self)))
    (let* ((name (pop form))
           (components 
             (mapcar 
              (lambda (f)
                (load-project-component 
                 (car f)
                 (if (= 1 (length (cdr f))) (cadr f) (cdr f)) 
                 :path (directory-path name)))
              form)))
      (setf (name self) name
            (components self) components)
      self)))

(defmethod initialize-instance :after ((self project-component) &key &allow-other-keys)
  (load-ast self))

(defcomponent project-module (project-component mod-component project-metadata) 
  ()
  (:keyword :mod)
  (:documentation "A module component for projects."))

(defmethod load-project-component ((kind (eql :mod)) (form t) &key (path *default-pathname-defaults*))
  (make-instance 'project-module :path path :ast (ensure-cons form)))

(defmethod project-compile ((self project-module) &key)
  (dolist (c (components self))
    (project-compile c)))

(defmethod build ((self project-module) &key)
  (dolist (c (components self))
    (build c)))

;;; Macros
(defwith project (name) (*project* (project name)))
(defwith rule (rule) (*rule* rule))

#+todo
(defmacro defproject (name &body body)
  "Like `defsys' for PROJECT instances.")
