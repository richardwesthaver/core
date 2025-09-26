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
(defvar *component-class-table* (make-hash-table))
(defvar *system-table* (make-hash-table)
  "An EQL hash-table containing NAME:SYSTEM pairs.")
(defvar *provider-table* (make-hash-table)
  "A hash-table containing PROVIDER functions.")
(defvar *defining-system* nil
  "When non-nil, indicates the name of the system currently being defined.")
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

(defun register-component-class (name class)
  (setf (gethash name *component-class-table*) class))

(defmacro defcomponent (name supers slots &rest opts)
  (let ((kw (find :keyword opts :key #'car)))
    (setf opts (delete :keyword opts :key #'car))
    `(prog1 (defclass* ,name ,(or supers '(component)) ,slots ,@opts)
       (register-component-class ,(cadr kw) (find-class ',name)))))

(defmethod make-load-form ((self component) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots self 
    :slot-names (mapcar 'sb-mop:slot-definition-name (class-slots (class-of self)))))

(defmethod print-object ((self component) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A ~A" (name self) (path self))))

(defcomponent file-component (component) 
  ((type :accessor component-type))
  (:keyword :file))

(defcomponent pkg-component (file-component) ()
  (:documentation "A FILE-COMPONENT which contains DEFPACKAGE-like forms.")
  (:keyword :pkg))

(defcomponent module-component (component) 
  ((components :accessor components))
  (:keyword :mod))

(defmethod print-object ((self module-component) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A ~A :components ~{~A~^ ~}" (name self) (path self) 
            (when (slot-boundp self 'components)
              (mapcar 'name (components self))))))

;;; Tasks
;; System Tasks are simple function which take a single component as an argument
(defkernel system-task (task) ())

;;; Jobs
;; System Jobs are effectively plans composed of system tasks
(defkernel system-job (job) ())

;;; Dependencies

;;; Providers
(eval-when (:compile-toplevel :load-toplevel)
  (defun register-provider (name function)
    (setf (gethash name *provider-table*) function)))

(defmacro defprovider (key args &body body)
  "Define a provider function which processes forms where the car is (eql KEY)."
  `(register-provider ,key (lambda ,args ,@body)))

(defun call-provider (name form)
  (when-let ((x (the function (gethash name *provider-table*))))
    (apply x form)))

(defprovider :tests (name &rest args)
  `(defsys ,name ,@args :class 'test-system))
(defprovider :bench (name &rest args)
  `(defsys ,name ,@args :class 'bench-system))
(defprovider :alien (name &rest args)
  `(std/alien:define-alien-loader ,name ,@args))
(defprovider :readtable (name)
  `(or (std/named-readtables:find-readtable ,name) ,name))
(defprovider :prelude (name &rest args)
  (if-let ((sys *defining-system*))
    `(pkg::%defpkg* ,sys (list ,name ,@args))
    name))
;; (defprovider :proto (name &rest args))

;;; System
(defcomponent system (module-component)
  ((version :accessor version)
   description
   provide
   require
   (hook :initform (make-instance 'key-hook) :accessor hook))
  (:keyword :sys))

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
     :name (keywordicate (string-upcase (asdf:component-name instance)))
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

;;; Test System
(defcomponent test-system (system) ())

(defmethod print-object ((self test-system) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A~@[ ~A~]" (name self) (version self))))

(defun test-system-name-p (name)
  (std/seq:ends-with-subseq "/TESTS" (string-upcase name)))

(definline %test-system-name (name)
  (concatenate 'simple-base-string (string-upcase name) "/TESTS"))

(defmethod change-class ((instance asdf:system) (new-class-name (eql 'test-system)) &key)
  (make-instance new-class-name
    :version (asdf:component-version instance)
    :name (keywordicate (string-upcase (asdf:component-name instance)))
    :path (asdf:component-pathname instance)
    :properties (asdf::component-properties instance)
    :description (asdf::component-description instance)
    :components (mapcar #'change-component-class (asdf:component-children instance))))

(defcomponent bench-system (system) ())

(defmethod change-class ((instance asdf:system) (new-class-name (eql 'bench-system)) &key)
  (make-instance new-class-name
    :version (asdf:component-version instance)
    :name (keywordicate (string-upcase (asdf:component-name instance)))
    :path (asdf:component-pathname instance)
    :properties (asdf::component-properties instance)
    :description (asdf::component-description instance)
    :components (mapcar #'change-component-class (asdf:component-children instance))))

(defun bench-system-name-p (name)
  (std/seq:ends-with-subseq "/BENCH" (string-upcase name)))

(definline %bench-system-name (name)
  (concatenate 'simple-base-string (string-upcase name) "/BENCH"))

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

(defun load-mod (name)
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
              `(load-mod ,core-mod)
              `(require ,mod))))))

;; TODO 2025-09-20: 
(defun partial-load-module (name &rest opts)
  (declare (ignore opts))
  (load-mod name))

(defun unload-module () (setf *module* nil))

(defun provide-core-module (name)
  "Provide a CORE-MODULE, adding valid entries to the *MODULES*
  variable. The function USE should be called in order to load and activate a
  module, but the deprecated PROVIDE function is also supported."
  (load-mod name))

(defmacro with-module (name &body body)
  "Load the module named NAME, binding it to *MODULE* and eval BODY."
  `(let ((*module* (or (load-module ,name) ,name)))
     ,@body))

;; (with-eval-after-load (module &body body))

;;; Session
(sb-ext:defglobal *system-session* nil
  "Global SYSTEM-SESSION or NIL when no systems have been initialized.")

(defvar *system-session-capacity* 64
  "The maximum count of systems which are allowed to wait in the systems queue for processing.")

(defstruct system-session
  "A reusable session in which SYSTEMs may be processed."
  ;; A queue of SYSTEMs to be processed, effectively a global stack.
  (systems (make-queue :capacity *system-session-capacity* :element-type 'system))
  ;; The set of PLAN objects which determine the work to be done on systems in the queue.
  (plans (make-priority-queue *system-session-capacity* :prioritize t :extend nil))
  ;; A simple cache of TASK results
  (task-cache (make-hash-table))
  ;; A simple cache of file operation times
  (file-cache (make-hash-table :test 'equal))
  ;; A thread-pool which is dedicated to running system tasks
  (pool (find-thread-pool :sys))
  ;; A queue of system tasks.
  tasks)

(defmacro with-system-session (&body body)
  "Bind *SYSTEM-SESSION* to a fresh value around BODY."
  `(progn
     (unless *system-session* (setf *system-session* (make-system-session)))
     ,@body))

;;; System Definition
(defun %parse-provide-form (form)
  (mapcar
   (lambda (x)
     (if (atom x) ; assumed to be a *FEATURE* keyword
         (progn 
           (pushnew x *features*) 
           x)
         (call-provider (car x) (cdr x))))
   form))

(defun %parse-require-form (form)
  (mapcar
   (lambda (x)
     (if (atom x) ; default case, load the module
         (load-mod x)
         (apply 'partial-load-module x)))
   form))

(defun %parse-component-form (form)
  (let ((n (cadr form))
        (kind (gethash (car form) *component-class-table*))
        (props (cddr form)))
    (ecase (car form)
      ((or :file :pkg) 
       (let ((ty (or (pathname-type n) "lisp")))
         (make-instance kind
           :type (keywordicate (string-upcase ty))
           :name n
           :path (truename (if ty (make-pathname :name n :type ty)))
           :properties props)))
      (:mod 
       (let* ((path (truename n))
              (*default-pathname-defaults* path))
         (make-instance kind 
           :name n 
           :properties props 
           :path path
           :components (mapcar '%parse-component-form (getf props :components))))))))

(defun %parse-components-form (form)
  (mapcar #'%parse-component-form form))

(defmacro defsys (name &body body)
  "Define a SYSTEM with NAME and BODY interpreted similar to ASDF:DEFSYSTEM.

SYSTEM objects register their own ASDF:SYSTEM objects as needed and provide
the following extensions:
- :PROVIDE    system-provided features, modules, readtables
- :HOOK       hook-spec to load with this system
- :METHODS    custom method definitions to apply to this system
- :REQUIRE    system-required modules and features"
  (flet ((%sys-get (n) 
           (when-let ((v (getf body n)))
             (remf body n)
             v)))
    (let ((prov (%sys-get :provide)) (hooks (%sys-get :hook))
          (meth (%sys-get :methods)) (req (%sys-get :require))
          (class (or (%sys-get :class) ''system))
          (comp (%sys-get :components))
          (*defining-system* name))
      (declare (ignore meth))
      (std/sym:with-gensyms (sys)
        `(let ((,sys (change-class (defsystem ,name ,@body) ,class)))
           (setf (path ,sys) *load-truename*
                 (slot-value ,sys 'components) `(,,@(%parse-components-form comp))
                 (slot-value ,sys 'provide) `(,,@(%parse-provide-form prov))
                 (slot-value ,sys 'require) `(,,@(%parse-require-form req)))
           (mapc (lambda (x) (add-hook (hook ,sys) x)) ',hooks)
           (register-system ,name ,sys)
           (eval-when (:execute)
             ,sys))))))

(defun load-sys (path &optional name)
  "Load a system definition from PATH. Unlike LOAD-ASD this function calls LOAD
internally. On success the path is added to the *SYSTEM-DEFINITIONS* list."
  (let ((path (truename path)))
    (with-system-session
      (let ((*default-pathname-defaults* (pathname (directory-namestring (namestring path)))))
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
(defmethod init ((self (eql :sys)) &key)
  (setq *system-table* (make-hash-table)
        *system-session* nil
        *system-definitions* nil
        *module* nil
        *module-stack* nil
        *module-table* (make-hash-table :test 'equal))
  (values))

(defgeneric register-system (name self)
  (:documentation "Register system SELF as NAME. This is called during DEFSYS.")
  (:method (name (self system))
    (with-system-session
      (setf (gethash name *system-table*) self))))

(defgeneric find-system (self &key &allow-other-keys)
  (:method ((self t) &key default)
    (multiple-value-bind (val found) (gethash (keywordicate self) *system-table*)
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
  (:method ((self system) &key force)
    (mumble "Loading system ~A~@[ from ~A~]" (name self) (path self))
    ;; TODO 2025-08-31: 
    (asdf:load-system (name self) :verbose nil :force force))
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

(defgeneric delete-system (self &key force &allow-other-keys)
  (:documentation "Delete the system SELF from the local filesystem."))

(defgeneric test-system (self &rest args)
  (:documentation "Test the system SELF."))

(defmethod test-system ((self system) &rest args)
  (mumble "Testing system ~A" (name self))
  (apply 'std:symbol-call :rt :do-suite (name self) args))

(defmethod test-system ((self symbol) &rest args)
  (let ((sys (find-system self :default :error)))
    (apply #'test-system sys args)))
