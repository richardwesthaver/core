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

(defhook *system-hooks* nil)

(defvar *system-table* (make-hash-table))

;;; Conditions
(define-condition defsys-condition () ())
(define-condition defsys-error (error defsys-condition) ())
(define-condition simple-defsys-error (simple-error defsys-condition) ())
(defun defsys-error (format &rest args)
  (error 'simple-defsys-error :format-control format :format-arguments args))

;;; Components
(defclass component () 
  ((name :initarg :name :accessor name)
   (path :initarg :path :accessor path)
   (properties :initarg :properties :accessor component-properties)))

(defmethod change-class ((instance asdf:component) (new-class-name (eql 'component)) &key)
  (make-instance new-class-name
    :name (asdf:component-name instance)))

(defclass module-component (component) 
  ((components :initarg :components :initform nil :accessor components)))

;;; Ops
;;; Actions
;;; Dependencies
;;; System
(defclass system (module-component)
  ((version :initarg :version :accessor system-version)
   (description :initarg :description :accessor system-description)
   (provides :initarg :provides :accessor system-provides)
   (requires :initarg :requires :accessor system-requires)
   (hooks :initform (make-instance 'key-hook) :initarg :hooks :accessor system-hooks)))

(defmethod change-class ((instance asdf:system) (new-class-name (eql 'system)) &key)
  (make-instance new-class-name
    :version (asdf:component-version instance)
    :name (asdf:component-name instance)
    :description (asdf:system-description instance)
    :components (asdf:component-children instance)))

;;; Modules
;; Unlike the MODULE object which is merely a container for other COMPONENTs,
;; Lisp Modules in the Core support the ANSI CL notion of Modules and are
;; further extended

(defvar *module* nil)
(defvar *module-stack* nil)
(defparameter *module-table* (make-hash-table :test 'equal))

(defclass core-module () 
  ((hook :type hook :accessor hook)))

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

;;; Plan
(defstruct system-plan
  "A set of parallel operations which are executed as a means of fulfilling a
specific method on a SYSTEM.")

;;; Session
(sb-ext:defglobal *system-session* nil
  "Global SYSTEM-SESSION or NIL when no systems have been initialized.")

(defstruct system-session
  "A reusable session in which SYSTEMs may be processed."
  (lock (make-mutex))
  (cache (make-hash-table))
  (pool *thread-pool*)
  (plan))

;;; System Definition
(defun %sys-get (form name)
  (std:when-let ((v (getf form name)))
    (remf form name)
    v))

(defmacro defsys (name &body body)
  "Define a SYSTEM with NAME and BODY interpreted similar to ASDF:DEFSYSTEM.

SYSTEM objects register their own ASDF:SYSTEM objects as needed and provide
the following extensions:
- :PROVIDE    system-provided features, modules, readtables
- :HOOKS       hooks to load with this system
- :METHODS     custom method definitions to apply to this system
- :REQUIRE    system-required modules and features"
  (let ((prov (%sys-get body :provide)) (hooks (%sys-get body :hooks))
        (meth (%sys-get body :methods)) (req (%sys-get body :require)))
    (declare (ignore meth))
    (std:with-gensyms (sys)
      `(let ((,sys (change-class (defsystem ,name ,@body) 'system)))
         ;; todo: convert to system
         (mapc (lambda (x) (pushnew x *features*)) ',prov)
         (mapc (lambda (x) (assert (member x *features*))) ',req)
         (mapc (lambda (x) (add-hook (system-hooks ,sys) x)) ',hooks)
         (register-system ,name ,sys)))))

;;; Protocol
(defgeneric register-system (name self)
  (:method (name (self system))
    (setf (gethash name *system-table*) self)))

(defgeneric find-system (self &key &allow-other-keys)
  (:method ((self symbol) &key)
    (gethash self *system-table*)))

(defgeneric remove-system (self &key &allow-other-keys)
  (:method ((self symbol) &key)
    (remhash self *system-table*)))

(defgeneric load-system (self &key &allow-other-keys))

(defgeneric compile-system (self &key &allow-other-keys))

(defgeneric make-system (self &key &allow-other-keys))
