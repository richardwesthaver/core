;;; defsys.lisp --- defsystem extension macros

;; The Core System Definition facility.

;;; Commentary:

;; replacement/wrapper for ASDF

;; goals:
;; - default to asdf (wrap)
;; - replace quicklisp (will need to be in lib/sys)
;; - share resources between system and dependency manager
;; - integrate with skel/packy (package distributor)
;; - multi-threaded by default
;; - parallel compilation (completely short-circuiting asdf)
;; - LISP ONLY -- multi-lang systems are handled by skel
;; notes:

;;; Code:
(in-package :std/defsys)

(defvar *system-definitions* nil
  "A list of files containing DEFSYS forms.")

(defvar *system-cache-directory* std/sys:*stash*)
(defvar *system-data-directory* nil)

(defvar *system-table* (make-hash-table)
  "An EQL hash-table containing NAME:SYSTEM pairs.")

(define-constant +system-definition-extension+ "sys" 
  :test 'equal
  :documentation "The default file extension used in system definitions.")

;;; Conditions
(define-condition system-condition () ())
(define-condition system-error (error system-condition) ())
(define-condition system-warning (warning system-condition) ())
(defwarning simple-system-warning (simple-warning system-warning) () (:auto t))
(deferror simple-system-error (simple-error system-condition) () (:auto t))

(define-condition sysdef-error (system-error file-error)
  ((system-name :initarg :name :accessor error-system-name))
  (:report (lambda (c s) 
             (format s "System ~A not found after loading file ~A" 
                     (error-system-name c) (file-error-pathname c)))))

;;; Components
(defclass component () 
  ((name :initarg :name :accessor name)
   (path :initarg :path :accessor path)
   (properties :initarg :properties :accessor component-properties)))

(defclass file-component (component) 
  ((type :initarg :type :reader component-type)))

(defclass module-component (component) 
  ((components :initarg :components :initform nil :accessor components)))

;;; Tasks
;; System Tasks are simple function which take a single component as an argument
(defkernel system-task (task) ())
;;; Jobs
;; System Jobs are effectively plans composed of system tasks
(defkernel system-job (job) ())

;;; Dependencies
;;; System
(defclass system (module-component)
  ((version :initarg :version :accessor version)
   (description :initarg :description :accessor system-description)
   (provide :initarg :provides :accessor system-provide)
   (require :initarg :requires :accessor system-require)
   (hook :initform (make-instance 'key-hook) :initarg :hooks :accessor hook)))

(defun system-equal (a b)
  "Return T if systems A and B refer to the same SYSTEM."
  (and (equal (name a) (name b))
       (equal (version a) (version b))
       (equal (path a) (path b))))

(defmethod add-hook ((self system) function &rest args)
  (apply 'add-hook (hook self) function args))

(defmethod print-object ((self system) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A~@[ ~A~]" (name self) (version self))))

;;; ASDF Compat
(definline change-component-class (self)
  "Change class of SELF to its associated STD/DEFSYS class."
  (etypecase self
    (asdf:system (change-class self 'system))
    (asdf:module (change-class self 'module-component))
    (asdf:file-component (change-class self 'file-component))
    (asdf:component (change-class self 'component))))

(definline revert-component-class (self)
  "Revert std/defsys class SELF to its associated ASDF class."
  (etypecase self
    (system (change-class self 'asdf:system))
    (module-component (change-class self 'asdf:module))
    (file-component (change-class self 'asdf:file-component))
    (component (change-class self 'asdf:component))))

(defmethods change-class 
  ;; components
  (((instance asdf:component) (new-class-name (eql 'component)) &key)
   (make-instance new-class-name
     :name (asdf:component-name instance)
     :path (asdf:component-pathname instance)
     :properties (asdf::component-properties instance)))
  (((instance component) (new-class-name (eql 'asdf:component)) &key)
   (make-instance new-class-name
     :name (name instance)
     :path (asdf:component-pathname instance)
     :properties (component-properties instance)))
  (((instance asdf:file-component) (new-class-name (eql 'file-component)) &key)
   (make-instance new-class-name
     :name (asdf:component-name instance)
     :path (asdf:component-pathname instance)
     :properties (asdf::component-properties instance)
     :type (asdf:file-type instance)))
  (((instance file-component) (new-class-name (eql 'asdf:file-component)) &key)
   (make-instance new-class-name
     :name (name instance)
     :path (asdf:component-pathname instance)
     :properties (component-properties instance)
     :type (component-type instance)))
  (((instance asdf:module) (new-class-name (eql 'module-component)) &key)
   (make-instance new-class-name
     :name (asdf:component-name instance)
     :path (asdf:component-pathname instance)
     :properties (asdf::component-properties instance)
     :components (mapcar #'change-component-class (asdf:component-children instance))))
  (((instance module-component) (new-class-name (eql 'asdf:module)) &key)
   (make-instance new-class-name
     :name (name instance)
     :path (asdf:component-pathname instance)
     :properties (component-properties instance)
     :components (mapcar #'revert-component-class (components instance))))
  ;; system
  (((instance asdf:system) (new-class-name (eql 'system)) &key)
   (make-instance new-class-name
     :version (asdf:component-version instance)
     :name (std/sym:keywordicate (string-upcase (asdf:component-name instance)))
     :path (asdf:component-pathname instance)
     :properties (asdf::component-properties instance)
     :description (asdf::component-description instance)
     :components (mapcar #'change-component-class (asdf:component-children instance))))
  (((instance system) (new-class-name (eql 'asdf:system)) &key)
   (warn 'simple-system-warning 
         :format-control "Erasing system slots (:require :provide :hook) from system ~A." 
         :format-arguments (name instance))
   (make-instance new-class-name
     :version (version instance)
     :name (name instance)
     :properties (component-properties instance)
     :description (system-description instance)
     :components (mapcar #'revert-component-class (components instance)))))

;;; Modules
;; Unlike MODULE-COMPONENT, based on ASDF:MODULE which is merely a container
;; for other COMPONENTs, Lisp Modules in the Core support the ANSI CL notion
;; of Modules and are further extended
(defvar *load-module* nil "The name of the module being loaded or NIL.")
(defvar *compile-module* nil "The name of the module being compiled or NIL.")
(defvar *module-stack* nil "A list of the most recently visited modules.")
(defvar *module* nil "The name of the current module or NIL.")
(defparameter *module-table* (make-hash-table :test 'equal)
  "A table which maps modules names to objects.")

(defclass module ()
  ((name :initarg :name :accessor name)
   (hook :initarg :hook :type hook :accessor hook))
  (:documentation "All Lisp Modules contain at least a NAME and HOOK slot."))

(defun load-core-module (name)
  (let ((cmod (gethash name *module-table*)))
    (with-slots (hook) cmod
      (when hook
        (pushnew (funcall hook :exit) sb-ext:*exit-hooks*)
        (funcall (funcall hook :load))))
    cmod))

(defmacro load-module (name)
  "Load module NAME from the global list *MODULES*."
  (let ((mod (find name *modules* :test 'string-equal)))
    (if (null mod) (warn "Module not found: ~A" name)
        (let ((core-mod (gethash mod *module-table*)))
           (if core-mod
               `(load-core-module ,core-mod)
               `(require ,mod))))))

(defun unload-module () (setf *module* nil))

(defun provide-core-module (name)
  "Provide a CORE-MODULE, adding valid entries to the *MODULES*
  variable. The function USE should be called in order to load and activate a
  module, but the deprecated PROVIDE function is also supported."
  (load-core-module name))

(defmacro with-module (name &body body)
  "Load the module named NAME, binding it to *MODULE* and eval BODY."
  `(let ((*module* (or (load-module ,name) ,name)))
     ,@body))

;; (with-eval-after-load (module &body body))

;;; Session
(sb-ext:defglobal *system-session* nil
  "Global SYSTEM-SESSION or NIL when no systems have been initialized.")

(defvar *system-session-capacity* 64)

(defstruct system-session
  "A reusable session in which SYSTEMs may be processed."
  (systems (make-queue :capacity *system-session-capacity* :element-type 'system))
  (plans (make-priority-queue *system-session-capacity* :prioritize t :extend nil))
  (task-cache (make-hash-table))
  (file-cache (make-hash-table :test 'equal))
  (pool (find-thread-pool :system-session))
  (tasks))

(defmacro with-system-session (&body body)
  "Bind *SYSTEM-SESSION* to a fresh value around BODY."
  `(progn
     (unless *system-session* (setf *system-session* (make-system-session)))
     ,@body))

;;; System Definition
(defun %sys-get (form name)
  (std/macs:when-let ((v (getf form name)))
    (remf form name)
    v))

(defmacro defsys (name &body body)
  "Define a SYSTEM with NAME and BODY interpreted similar to ASDF:DEFSYSTEM.

SYSTEM objects register their own ASDF:SYSTEM objects as needed and provide
the following extensions:
- :PROVIDE    system-provided features, modules, readtables
- :HOOK       hook-spec to load with this system
- :METHODS    custom method definitions to apply to this system
- :REQUIRE    system-required modules and features"
  (let ((prov (%sys-get body :provide)) (hooks (%sys-get body :hook))
        (meth (%sys-get body :methods)) (req (%sys-get body :require)))
    (declare (ignore meth))
    (std/sym:with-gensyms (sys)
      `(let ((,sys (change-class (defsystem ,name ,@body) 'system)))
         (setf (path ,sys) *load-truename*)
         ;; todo: convert to system
         (mapc (lambda (x) (pushnew x *features*)) ',prov)
         (mapc (lambda (x) (assert (member x *features*))) ',req)
         (mapc (lambda (x) (add-hook (hook ,sys) x)) ',hooks)
         (register-system ,name ,sys)))))

(defun load-sys (path &optional name)
  "Load a system definition from PATH. Unlike LOAD-ASD this function calls LOAD
internally. On success the path is added to the *SYSTEM-DEFINITIONS* list."
  (let ((path (truename path)))
    (with-system-session
      (let ((*default-pathname-defaults* (std/path:directory-path path)))
        (when 
            (restart-case (load path)
              (load-file (p)
                :report "Load a different file." 
                :interactive (lambda () 
                               (list (setf path (interact-line "File: "))))
                (load p)))
          (setf (gethash path (system-session-file-cache *system-session*))
                (sb-ext:get-time-of-day))
          (pushnew (namestring (truename path)) *system-definitions* :test 'equal)
          (if name 
              (find-system name :default (lambda () (error 'sysdef-error :name name :pathname path)))
              t))))))

(defmethod serde ((from system) (to stream)))

;;; Protocol
(defgeneric register-system (name self)
  (:documentation "Register system SELF as NAME. This is called during DEFSYS.")
  (:method (name (self system))
    (with-system-session
      (setf (gethash name *system-table*) self))))

(defgeneric find-system (self &key &allow-other-keys)
  (:method ((self symbol) &key default)
    (multiple-value-bind (val found) (gethash self *system-table*)
      (cond
        (found (values val found))
        ((eql default :error) (simple-system-error "System ~A not found." self))
        ((functionp default) (funcall default))
        (t default)))))

(defgeneric remove-system (self &key &allow-other-keys)
  (:method ((self symbol) &key)
    (with-system-session
      ;; freeze the session by acquiring the queue lock
      (with-queue-lock (system-session-systems *system-session*) 
        (remhash self *system-table*)))))

(defgeneric load-system (self &key &allow-other-keys)
  (:documentation "Load the system SELF by ensuring all dependencies and components are loaded.")
  (:method ((self system) &key)
    (mumble "Loading system ~A~@[ from ~A~]" (name self) (path self))
    ;; TODO 2025-08-31: 
    (asdf:load-system (name self) :verbose nil))
  (:method ((self symbol) &key)
    (let ((sys (find-system self :default :error)))
      (load-system sys))))

(defgeneric compile-system (self &key &allow-other-keys)
  (:documentation "Compile system SELF.")
  (:method ((self system) &key)
    (mumble "Compiling system ~A" (name self))
    (asdf:compile-system self :verbose nil))
  (:method ((self symbol) &key)
    (let ((sys (find-system self :default :error)))
      (compile-system sys))))

(defgeneric save-system (self &key &allow-other-keys)
  (:documentation "Save the system SELF."))

(defgeneric make-system (self &key &allow-other-keys)
  (:documentation "Make the system SELF which usually entails loading, compiling, and then saving
an image.")
  (:method ((self system) &key)
    (mumble "Making system ~A" (name self))
    (asdf:make self :verbose nil))
  (:method ((self symbol) &key)
    (let ((sys (find-system self :default :error)))
      (make-system sys))))

(defgeneric fetch-system (self &key &allow-other-keys)
  (:documentation "Fetch a system SELF from a remote location."))

(defgeneric update-system (self &key &allow-other-keys)
  (:documentation "Update the system SELF."))

(defgeneric delete-system (self &key &allow-other-keys)
  (:documentation "Delete the system SELF from the local filesystem."))

(defgeneric test-system (self &rest args)
  (:documentation "Test the system SELF.")
  (:method ((self system) &key)
    (mumble "Testing system ~A" (name self))
    (asdf:test-system self :verbose nil))
  (:method ((self symbol) &key)
    (let ((sys (find-system self :default :error)))
      (test-system sys))))

(defgeneric bench-system (self &key &allow-other-keys)
  (:documentation "Benchmark the system SELF."))
