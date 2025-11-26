;;; defsys.lisp --- defsystem extension macros

;; The Core System Definition facility.

;;; Commentary:

;; replacement/wrapper for ASDF

;; goals:
;; - dynamic asdf compatibility
;; - replace quicklisp (will need to be in lib/sys)
;; - share resources between system and dependency manager
;; - integrate with skel/packy (package distributor)
;; - multi-threaded by default
;; - parallel compilation (completely short-circuiting asdf)
;; - LISP ONLY -- multi-lang systems are handled by skel
;; notes:

;; operations we care about:
;; - read
;; - load
;; - compile

;;; Code:
(in-package :std/defsys)
(in-readtable :std)

(defvar *sysdefs* nil
  "A list of files containing DEFSYS forms.")

(defvar *system-cache-directory* #l"user:stash;cache;lisp;sys;")
(defvar *system-data-directory* #l"user:stash;data;lisp;sys;")

(defvar *component-class-table* (make-hash-table))
(defvar *test-system* :rt)
(defvar *system-table* (make-hash-table)
  "An EQL hash-table containing NAME:SYSTEM pairs.")

(defvar *provider-table* (make-hash-table)
  "A hash-table containing PROVIDER functions.")

(defvar *defining-system* nil
  "When non-nil, indicates the name of the system currently being defined.")

(defvar *asdf-compatibility* nil
  "When non-nil, enable compatibility between STD/DEFSYS and SYSTTEM - component
operations will use ASDF and DEFSYS will first pass all argument to
ASDF:DEFSYSTEM.")

(define-constant +sys-extension+ "sys" 
  :test 'string=
  :documentation "The default file extension used in system definitions.")

;;; Conditions
(define-condition system-condition () ())
(define-condition system-error (error system-condition) ())
(define-condition system-warning (warning system-condition) ())
(defwarning simple-system-warning (simple-warning system-warning) () (:auto t))
(deferror simple-system-error (simple-error system-condition) () (:auto t))

(deferror defsys-load-error (system-error file-error)
  ((system-name :initarg :name :accessor error-system-name))
  (:report (lambda (c s) 
             (format s "System ~A not found after loading file ~A" 
                     (error-system-name c) (file-error-pathname c)))))

;;; Sysdef Utils
;; system definitions are files ending with +SYS-EXTENSION+ containing lisp
;; code.
(defun sysdefs (&optional (dir *default-pathname-defaults*) (recurse t))
  "Return a list of system definition pathnames found in DIR."
  (collecting
    (walk-directory dir 
      (constantly t)
      (constantly recurse)
      (lambda (x) 
        (mapc
         #'collect
         (directory-files x #.(format nil "*.~A" +sys-extension+)))))))

(defun sysdef (&optional (dir *default-pathname-defaults*))
  "Return the 'default' system definition path of the current directory, if it exists."
  (when-let ((defs (sysdefs dir)))
    (if (= 1 (length defs))
        (car defs)
        (find (last (pathname-directory dir)) defs :test 'string-equal :key 'pathname-name))))

(defun list-all-systems ()
  (std/hash:hash-table-values *system-table*))

(defun list-all-modules ()
  (std/hash:hash-table-alist *module-table*))

(defun list-all-providers ()
  (std/hash:hash-table-alist *provider-table*))

;;; Components
(defclass component () 
  ((name :initarg :name :accessor name)
   (path :initarg :path :accessor path)))

(defun register-component-class (name class)
  (setf (gethash name *component-class-table*) class))

(defmacro defcomponent (name supers slots &rest opts)
  (let ((kw (find :keyword opts :key 'car)))
    (setf opts (delete :keyword opts :key 'car))
    `(prog1 (defclass* ,name ,(or supers '(component)) ,slots ,@opts)
       (register-component-class ,(cadr kw) (find-class ',name)))))

(defmethod make-load-form ((self component) &optional env)
  (make-load-form-saving-slots self
    :slot-names (mapcar 'sb-mop:slot-definition-name (class-slots (class-of self)))
    :environment env))

(defmethod print-object ((self component) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (name self))))

(defcomponent file-component (component) 
  ((type :accessor component-type))
  (:keyword :file))

(defcomponent pkg-component (file-component) ()
  (:documentation "A FILE-COMPONENT which contains DEFPACKAGE-like forms.")
  (:keyword :pkg))

(defcomponent mod-component (component) 
  ((components :accessor components))
  (:keyword :mod))

(defun mod-component-p (c)
  (typep c 'mod-component))

(defmethod print-object ((self mod-component) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A ~A :components ~{~A~^ ~}" (name self) (path self) 
            (when (slot-boundp self 'components)
              (mapcar 'name (components self))))))

(defcomponent dir-component (mod-component) 
  ((include :accessor component-include)
   (exclude :accessor component-exclude))
  (:documentation "A MOD-COMPONENT which matches regexp patterns against all files in a
directory recursively.")
  (:keyword :dir))

;; TODO 2025-11-18: full compatibility with SB-GROVEL interface to ASDF
(defcomponent grovel-component (file-component) 
  (package)
  (:documentation "A FILE-COMPONENT which matches a SB-GROVEL constants file.")
  (:keyword :grovel))

;;; Component Ops
;; Functions which are performed directly on instances of the COMPONENT class
;; in the calling thread.
(defun read-component (comp &key (external-format :default))
  "Read a component from its PATH slot."
  (etypecase comp
    (mod-component (mapcar 'read-component (components comp)))
    (component (read-lisp-file (path comp) :external-format external-format))
    (pathname (read-lisp-file comp :external-format external-format))))

(defun compile-component (comp &rest args)
  "Compile a component."
  (etypecase comp
    (mod-component (mapcar 'compile-component (components comp)))
    (component (apply 'std/comp:checked-compile-file (path comp) args))
    (pathname (apply 'std/comp:checked-compile-file comp args))))

(defun load-component (comp &rest args)
  "Load a component."
  (etypecase comp
    (mod-component (mapcar 'load-component (components comp)))
    ;; TODO
    (grovel-component (if-let ((pkg (find-package (slot-value comp 'package))))
                        (setf (slot-value comp 'package) pkg)
                        (std-error "Missing package (~A) for grovel component ~A" (slot-value comp 'package) comp)))
    (component (apply 'load (path comp) args))
    (pathname (apply 'load comp args))))

(defun find-component (path self)
  "Find a component designated by PATH which is either an atom designating a
component name or a list indicating a sequence of module component names
ending with the target component name."
  (declare (component self))
  (if (atom path)
      (find path (components self) :test 'string-equal :key 'name)
      (let ((c self))
        (loop for p in path
              do (setf c (find p (components c) :test 'string-equal :key 'name))
              finally (return c)))))

;;; Tasks
;; System Tasks are simple function which take a single component as an argument
(defkernel system-task (task) ())

;;; Jobs
;; System Jobs are effectively plans composed of system tasks
(defkernel system-job (job system-task) ())

;;; Provider
(eval-when (:compile-toplevel :load-toplevel)
  (defun register-provider (name function)
    (setf (gethash name *provider-table*) function)))

(defun find-provider (key)
  (gethash key *provider-table*))

(defmacro defprovider (key args &body body)
  "Define a provider function which processes forms where the car is (eql KEY)."
  `(register-provider ,key (lambda ,args ,@body)))

(defun call-provider (name form)
  (when-let ((x (the function (find-provider name))))
    (apply x form)))

(defun register-module (key name val &optional (replace t))
  (let ((form (list key val)))
    (multiple-value-bind (v f) (gethash name *module-table*)
      (if f
          (setf (gethash name *module-table*) ; v is a plist
                (let ((w (or (and replace (apply 'std/list:remove-from-plist v (list key))) v)))
                  (nconc w form)))
          (setf (gethash name *module-table*) form)))))

(defprovider :alien (name &rest args)
  (register-module :alien name (compile-and-eval `(std/alien:define-alien-loader ,name ,@args))))

(defprovider :readtable (name)
  (register-module :readtable name `(std/named-readtables:find-readtable ,name)))

(defprovider :prelude (name &rest args)
  (let ((root *current-module-name*))
    (register-module 
     :prelude
     name
     `(pkg::%defpkg* ,root (list ,name ,@args)))))

(defprovider :io (name &rest args)
  (register-module :io name args))

(defprovider :proto (name &rest args)
  (register-module :proto name args))

(defprovider :pool (name)
  (register-module :pool name `(find-thread-pool ,name)))

;; TODO 2025-09-28: 
(defprovider :module (name &rest args)
  (compile-and-eval 
   `(defmodule ,name ,@args)))

(defprovider :sys (name &rest args)
  (register-module 
   :sys
   name 
   (compile-and-eval 
    `(defsys ,name ,@args :path ,(or *compile-file-truename* *load-truename* (path (find-system name)))))))

(defprovider :tests (name &rest args)
  (let ((req (getf args :require)))
    (remf args :require)
    (unless (member name req :test 'string-equal)
      (push name req))
    (unless (member *test-system* req :test 'string-equal)
      (push *test-system* req))
  (register-module 
   :tests
   name 
   (compile-and-eval 
    `(defsys ,(%test-system-name name) ,@args 
       :require ,req :class 'test-system 
       :path ,(or *compile-file-truename* 
                  *load-truename* (path (find-system name))))))))

(defprovider :bench (name &rest args)
  (register-module 
   :bench name 
   `(defsys ,(%bench-system-name name) 
      ,@args 
      :class 'bench-system 
      :path ,(or *compile-file-truename* *load-truename*))))

;;; Module
;; Unlike MOD-COMPONENT/DIR-COMPONENT, based on ASDF:MODULE which is merely a
;; container for other COMPONENTs, Lisp MODULEs in the Core support the ANSI
;; CL notion of Modules and are further extended.

;; Modules in the core are essentially a 1:N mapping from an arbitrary name
;; (string or symbol) to tagged lisp objects we call providers. Providers are
;; designated by a keyword (the tag) and are responsible for returning a form
;; which will be evaluated on a call to INIT.

;; The REQUIRE slot is a list of provider forms which indicate the
;; dependencies of the module. A call to INIT will parse each element
;; individually and attempt to load any dependencies indicated into the
;; current image.

;; Note that calling INIT on a SYSTEM or MODULE is not the same as loading it
;; - the idea is that INIT prepares the current image so that operations don't
;; need to concern themselves with checking for external dependencies. Note
;; that internal dependencies still need to be coordinated between operations
;; - that's what the system plan is for.

;; Both of these slots
(defvar *load-module* nil "The name of the module being loaded or NIL.")
(defvar *compile-module* nil "The name of the module being compiled or NIL.")
(defvar *module-stack* nil "A list of modules to be visited.")
(defvar *module* nil "The name of the current module or NIL.")
(defparameter *module-table* (make-hash-table :test 'equal)
  "A table which maps modules names to objects.")

(defclass module ()
  ((name :initarg :name :accessor name)
   (hook :initarg :hook :type hook :accessor hook)
   (provide :initarg :provide :accessor module-provide)
   (require :initarg :require :accessor module-require))
  (:documentation "All Lisp Modules contain at least a NAME, HOOK, PROVIDE and REQUIRE slot."))

(defun find-module* (name)
  (or (gethash name *module-table*)
      (when-let ((sys (gethash name *system-table*)))
        (init sys)
        (gethash name *module-table*)))) ; no recurse

(defun find-module (name &optional kind)
  (when-let ((mod (find-module* name)))
    (if kind
        (getf mod kind)
        mod)))

(defun set-module (name val &optional kind (replace t))
  (if kind
      (register-module kind name val replace)
      (setf (gethash name *module-table*) val)))

(defsetf find-module (name &optional kind (replace t)) (val)
  `(set-module ,name ,val ,kind ,replace))

(defmacro with-module-hooks (mod)
  (with-slots (hook) mod
    (when hook
      (pushnew (funcall hook :exit) sb-ext:*exit-hooks*)
      (funcall (funcall hook :load)))))

(defvar *current-module-name* nil)
;; TODO 2025-11-18: memoizing might be useful here..
;; templates?
(defun %load-module (form &optional kind)
  (case kind
    ((or :alien :io :proto) form) ; non-evaluated
    (:prelude (compile-and-eval form))
    (:tests (load-system form))
    (:bench (load-system form))
    (:readtable (eval form))
    (t
     (sb-int:doplist (k v) form
       (%load-module v k)))))

(defun load-module (name &optional kind)
  (let ((*current-module-name* name))
    (when-let ((*load-module* (find-module name kind)))
      (%load-module *load-module* kind))))

;; TODO 2025-09-20: 
(defun partial-load-module (name &rest args)
  (if args 
      (nyi!)
      (load-module name)))

(defun unload-module (name)
  (when (eql *module* name)
    (setf *module* nil))
  (nyi!))

(defun partial-unload-module (name &rest args)
  (declare (ignore args))
  (unload-module name))

(defun module-provide-system (name)
  "Provide a SYSTEM, adding valid entries to the *MODULES* variable. The function
USE should be called in order to load and activate a module."
  (when-let ((sys (find-system name)))
    (load-system sys)
    t))

(defmacro with-module (name &body body)
  "Load the module named NAME, binding it to *MODULE* and eval BODY."
  `(let ((*module* (or (require ,name) ,name)))
     ,@body))

(defmacro use (name &body body)
  "Load and activate a package or module by NAME with the provider forms in BODY."
  (if body
      `(partial-load-module ,name ,@body)
      (if (find-package name)
          `(use-package ,name)
          `(load-module ,name))))

(defmacro using (&rest args)
  `(progn
     ,@(mapcar (lambda (x) (if (atom x) `(use ,x) `(use ,@x))) args)))

(defmacro refuse (name &body body)
  "Unload and deactivate a package or module by NAME modulo BODY."
  (if body
      `(partial-unload-module ,name ,@body)
      (if (find-package name)
          `(unuse-package ,name)
          `(unload-module ,name))))

(defmacro refusing (&rest args)
  `(progn
     ,@(mapcar (lambda (x) (if (atom x) `(refuse ,x) `(refuse ,@x))) args)))

;; (with-eval-after-load (module &body body))

;;; System
(defcomponent system (mod-component module)
  ((version :accessor version :initform nil)
   description
   (plan 
    :documentation "The default plan associated with this object which specifies the ordering of
system jobs to be executed in an async context."
    :initform :serial
    :accessor plan))
  (:keyword :sys)
  (:default-initargs :hook (make-instance 'key-hook))
  (:documentation "Base class for system definitions found throughout the core (*.sys)."))

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
    (asdf:module (change-class self 'mod-component))
    (asdf:file-component (change-class self 'file-component))
    (asdf:component (change-class self 'component))))

(definline revert-component-class (self)
  "Revert std/defsys class SELF to its associated ASDF class."
  (etypecase self
    (system (change-class self 'asdf:system))
    (mod-component (change-class self 'asdf:module))
    (grovel-component (change-class self 'sb-grovel:grovel-constants-file))
    (file-component (change-class self 'asdf:file-component))
    (component (change-class self 'asdf:component))))

(defmethods change-class 
  ;; components
  (((instance asdf:component) (new-class-name (eql 'component)) &key)
   (make-instance new-class-name
     :name (asdf:component-name instance)
     :path (asdf:component-pathname instance)))
  (((instance component) (new-class-name (eql 'asdf:component)) &key)
   (make-instance new-class-name
     :name (name instance)
     :path (asdf:component-pathname instance)))
  (((instance asdf:file-component) (new-class-name (eql 'file-component)) &key)
   (make-instance new-class-name
     :name (asdf:component-name instance)
     :path (asdf:component-pathname instance)
     :type (asdf:file-type instance)))
  (((instance file-component) (new-class-name (eql 'asdf:file-component)) &key)
   (make-instance new-class-name
     :name (name instance)
     :path (path instance)
     :type (component-type instance)))
  (((instance grovel-component) (new-class-name (eql 'sb-grovel:grovel-constants-file)) &key)
   (make-instance new-class-name
     :name (name instance)
     :path (path instance)
     :type (component-type instance)
     :package (grovel-component-package instance)))
  (((instance sb-grovel:grovel-constants-file) (new-class-name (eql 'grovel-component)) &key)
   (make-instance new-class-name
     :name (asdf:component-name instance)
     :path (asdf:component-pathname instance)
     :type (asdf:file-type instance)
     :package (grovel-component-package instance)))
  (((instance asdf:module) (new-class-name (eql 'mod-component)) &key)
   (make-instance new-class-name
     :name (asdf:component-name instance)
     :path (asdf:component-pathname instance)
     :components (mapcar 'change-component-class (asdf:component-children instance))))
  (((instance mod-component) (new-class-name (eql 'asdf:module)) &key)
   (make-instance new-class-name
     :name (name instance)
     :path (asdf:component-pathname instance)
     :components (mapcar 'revert-component-class (components instance))))
  ;; system
  (((instance asdf:system) (new-class-name (eql 'system)) &key)
   (make-instance new-class-name
     :version (asdf:component-version instance)
     :name (keywordicate (string-upcase (asdf:component-name instance)))
     :path (asdf:component-pathname instance)
     :description (asdf::component-description instance)
     :components (mapcar 'change-component-class (asdf:component-children instance))))
  (((instance system) (new-class-name (eql 'asdf:system)) &key)
   (warn 'simple-system-warning 
         :format-control "Erasing system slots (:require :provide :hook) from system ~A." 
         :format-arguments (name instance))
   (make-instance new-class-name
     :version (version instance)
     :name (name instance)
     :description (system-description instance)
     :components (mapcar 'revert-component-class (components instance)))))

;;; Test System
(defcomponent test-system (system) ()
  (:keyword :tests))

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
    :description (asdf::component-description instance)
    :components (mapcar 'change-component-class (asdf:component-children instance))))

;;;; Bench System
(defcomponent bench-system (system) ()
  (:keyword :bench))

(defmethod change-class ((instance asdf:system) (new-class-name (eql 'bench-system)) &key)
  (make-instance new-class-name
    :version (asdf:component-version instance)
    :name (keywordicate (string-upcase (asdf:component-name instance)))
    :path (asdf:component-pathname instance)
    :description (asdf::component-description instance)
    :components (mapcar 'change-component-class (asdf:component-children instance))))

(defun bench-system-name-p (name)
  (std/seq:ends-with-subseq "/BENCH" (string-upcase name)))

(definline %bench-system-name (name)
  (concatenate 'simple-base-string (string-upcase name) "/BENCH"))

;; preloaded
;; immutable

;;; Session
(eval-always
  (defvar *system-session-capacity* 64
    "The maximum count of systems which are allowed to wait in the systems queue for processing.")
  (defstruct system-session
    "A reusable session in which SYSTEMs may be processed."
    ;; A queue of SYSTEMs to be processed, effectively a global stack.
    (systems (make-queue :capacity *system-session-capacity* :element-type 'system))
    ;; A simple cache of TASK results
    (task-cache (make-hash-table))
    ;; A simple cache of file operation times
    (file-cache (make-hash-table :test 'equal))
    ;; A thread-pool which is dedicated to running system tasks
    (pool (make-thread-pool (std/alien:num-cpus) :name :sys :alive nil))
    ;; A queue of system tasks.
    tasks))

(defmethod start ((self system-session))
  (start (system-session-pool self)))

(defmethod stop ((self system-session) &key)
  (stop (system-session-pool self)))

(sb-ext:defglobal *system-session* nil
    "Global SYSTEM-SESSION or NIL when no systems have been initialized.")

(defmacro with-system-session ((&optional sys) &body body)
  "Bind *SYSTEM-SESSION* to a fresh value around BODY."
  `(progn
     (unless *system-session* (setf *system-session* (make-system-session)))
     . ,(if sys
            `((let ((*default-pathname-defaults* (if (pathnamep ,sys) ,sys
                                                     (pathname (directory-namestring (probe-file (path ,sys)))))))
                ,@body))
            `(,@body))))

;;; Defsys
(defun %parse-provide-form (form)
  (let ((ret))
    (mapc
     (lambda (x)
       (if (atom x) ; return as is
           (pushnew (string-upcase x) *modules* :test 'string-equal)
           (push x ret)))
     form)
    (nreverse ret)))

#+nil
(defun %parse-require-form (form)
  (mapcar
   (lambda (x)
     (if (atom x)
         (load-system x :asdf t) ; default case, assumed to be a system
         (apply 'partial-load-module x))
     x)
   form))

(defvar *wildcard-regexp* (cl-ppcre:create-scanner ".*"))

(defun %mod-component-walk (c &optional inc exc)
  (walk-directory (path c)
    (constantly t) (constantly t)
    (lambda (x)
      (dolist (f (directory-files x))
        (let ((f (namestring f))) ; set name only
          (when (and inc (cl-ppcre:scan inc f) (or (not exc) (not (cl-ppcre:scan exc f))))
            (push (%parse-component-form f) (components c)))))))
  ;; fill in the path
  (mapc (lambda (x) 
          (setf (path x) 
                (probe-file (make-pathname 
                             :name (name x) 
                             :type (string-downcase (component-type x) )
                             :directory (namestring (path c))))))
        (components c))
  c)


(defun %parse-component-form (form)
  (if (atom form) ; atoms will populate a NAME and TYPE but not a PATH
      (if (directory-path-p form)
          (make-instance 'dir-component
            :include ".*" 
            :name (last (pathname-directory form)))
          (make-instance 'file-component 
            :type (or (pathname-type form) "lisp")
            :name (pathname-name form)))
      (let ((n (cadr form))
            (kind (gethash (car form) *component-class-table*))
            (props (cddr form)))
        (ecase (car form)
          ((or :file :pkg :grovel)
           (let ((ty (or (pathname-type n) "lisp")))
             (apply 'make-instance kind
                    :type (keywordicate (string-upcase ty))
                    :name n
                    :path (probe-file (make-pathname :name n :type ty :defaults *default-pathname-defaults*))
                    props)))
          (:mod
           (let* ((path (std/file:probe-directory (make-pathname :name n :defaults *default-pathname-defaults*)))
                  (c (make-instance kind
                       :name n 
                       :path path
                       :components (mapcar '%parse-component-form (getf props :components)))))
             (%mod-component-walk c)))
          (:dir
           (let* ((path (std/file:probe-directory (make-pathname :name n :defaults *default-pathname-defaults*)))
                  (inc (or (getf props :include) *wildcard-regexp*))
                  (exc (getf props :exclude))
                  (c (make-instance kind
                       :name n
                       :path path
                       :include inc
                       :exclude exc
                       :components (mapcar '%parse-component-form (getf props :components)))))
             (%mod-component-walk c inc exc)))))))

(defun %parse-components-form (form)
  (mapcar '%parse-component-form form))

(defmacro %make-sys (name class &body args)
  `(progn
     (if *asdf-compatibility*
         (change-class (defsystem ,name . ,args) ,class)
         ;; args are ignored if *ASDF-COMPATIBILITY* is nil. Make sure to fill
         ;; in the slots of the return value.
          (make-instance ,class :name ,name . ,args))))

(defun %sys-get (n body)
  (when-let ((v (getf body n)))
    (remf body n)
    v))

(defmacro defsys (name &body body)
  "Define a SYSTEM with NAME and BODY interpreted similar to ASDF:DEFSYSTEM.

SYSTEM objects register their own ASDF:SYSTEM objects as needed and provide
the following extensions:
- :PROVIDE    system-provided features, modules, readtables
- :HOOK       hook-spec to load with this system
- :REQUIRE    system-required modules and features"
  (multiple-value-bind (%body dec doc) (std-int:parse-body body :documentation t)
    (declare (ignore dec))
    (let ((prov (%sys-get :provide %body)) (hooks (%sys-get :hook %body))
          (path (%sys-get :path %body))
          (req (%sys-get :require %body))
          (ver (%sys-get :version %body))
          (plan (or (%sys-get :plan %body) :serial))
          (class (or (%sys-get :class %body) ''system))
          (comp (%sys-get :components %body))
          (*defining-system* name))
      (std/sym:with-gensyms (sys)
        `(let ((,sys (apply 'make-instance ,class :name ,name ',%body)))
           (when-let ((fpath (or *compile-file-truename* *load-truename* ,path)))
             (setf *default-pathname-defaults* (make-pathname :directory (pathname-directory fpath))))
           (let ((*defining-system* ,sys))
             (setf (path ,sys) (or ,path *compile-file-truename* *load-truename*)
                   (slot-value ,sys 'plan) ,plan
                   (slot-value ,sys 'description) ,doc
                   (slot-value ,sys 'version) ,ver
                   (slot-value ,sys 'components) (%parse-components-form ',comp)
                   (slot-value ,sys 'provide) (%parse-provide-form ',prov)
                   (slot-value ,sys 'require) ',req)
             (mapc (lambda (x) (add-hook (hook ,sys) x)) ',hooks)
             ;; (inspect ,sys)
             (register-system ,name ,sys)
             ,sys))))))

(defun compile-sys (path &optional output-file)
  "Compile a system's defsys file by PATH. Default extension is FSYS."
  (unless (pathnamep path) (setf path (or (when-let ((sys (find-system path))) (path sys)) (pathname path))))
  (checked-compile-file path
                        :output-file (or output-file (make-pathname :name (pathname-name path) :type "fsys"))
                        :entry-points '(load-sys)))

(defun load-sys (path &optional name)
  "Load a SYS file from PATH. Unlike LOAD-ASD this function calls LOAD
internally. On success the path is added to the *SYSDEFS* list."
  (let ((path (etypecase path
                ((or string pathname) (truename path))
                (t (find path *sysdefs* :key 'pathname-name :test 'string-equal)))))
    (unless (string-equal (pathname-type path) "fsys")
      (when-let ((compiled (probe-file (make-pathname :defaults path :type "fsys"))))
        (setf path compiled)))
    ;; (mumble "loading systems from ~A" path)
    (with-system-session ((pathname (directory-namestring path)))
      (when 
          (restart-case (load path)
            (load-file (p)
              :report "Load a different file." 
              :interactive (lambda () 
                             (list (setf path (interact-line "File: "))))
              (load p)))
        (setf (gethash path (system-session-file-cache *system-session*))
              (sb-ext:get-time-of-day))
        (pushnew path *sysdefs* :test 'equal)
        (if name 
            (find-system name :default (lambda () (error 'defsys-load-error :name name :pathname path)))
            t)))))

;;; Protocol
(defmethod init ((self (eql :sys)) &key (sysdefs (sysdefs)) (preload t))
  "Initialize STD/DEFSYS variables given a list of system directories SYSDEFS and
optionally calling LOAD-SYS on them when PRELOAD is T (default)."
  (when sysdefs (setq *sysdefs* sysdefs))
  (setq *system-table* (make-hash-table)
        *system-session* nil        
        *module* nil
        *module-stack* nil
        *module-table* (make-hash-table :test 'equal))
  (ensure-directories-exist *system-data-directory*)
  (ensure-directories-exist *system-cache-directory*)
  (pushnew 'std/defsys::module-provide-system sb-ext:*module-provider-functions*)
  (when (and sysdefs preload) (mapc 'load-sys *sysdefs*))
  (values))

(defmethod init ((self system) &key)
  "Initialize a SYSTEM which has been pre-loaded with LOAD-SYS. Arrange for
REQUIRE forms, PKG components, and some PROVIDE forms to be loaded. The
underlying object SELF remains unmodified."
  (with-system-session (self)
    (let ((*current-module-name* (name self)))
      ;; first process all requires
      (mapc
       (lambda (x)
         (if (atom x)
             (load-system x :asdf t) ; default case, assumed to be a system
             (apply 'load-module x)))
       (slot-value self 'require))
      ;; then we call providers
      (mapc (lambda (x) (call-provider (car x) (cdr x)))
            (slot-value self 'provide))
      t)))
  ;; (typecase x
  ;;   ;; symbols and strings use PROVIDE
  ;;   ((or symbol simple-string) (provide x) x)
  ;;   ;; WARNING: use of eval
  ;;   (list (with-safe-io-syntax (:std/defsys) (eval (cdr x))))
  ;;   ;; otherwise return as-is
  ;;   (t x)))
  ;; (slot-value self 'provide)))

(defun expand-component-paths (c)
  "Walk the components of C, expanding PATH slots along the way to
absolute pathnames. Shouldn't be needed if all system components exist when
LOAD-SYS is called."
  (labels ((.expand (comp)
             (when (probe-file (path comp)) (setf (path comp) (probe-file (path comp))))
             (when (and (mod-component-p comp) (components comp))
               (let ((*default-pathname-defaults* (path comp)))
                 (mapc #'.expand (components comp))))))
    (declare (dynamic-extent (function .expand))
             (optimize (speed 3) (safety 0)))
    (mapc (the (function (component) (values)) #'.expand) (components c))))

(defgeneric component-dependencies (task component))

(defgeneric register-system (name self)
  (:documentation "Register system SELF as NAME. This is called during DEFSYS.")
  (:method (name self)
    (with-system-session (self)
      (setf (gethash name *system-table*) self))))

(defgeneric find-system (self &key &allow-other-keys)
  (:method ((self t) &key default (asdf *asdf-compatibility*))
    (multiple-value-bind (val found) (gethash (keywordicate (string-upcase self)) *system-table*)
      (cond
        (found (values val found))
        (asdf (asdf:find-system self (eql default :error)))
        ((eql default :error) (simple-system-error "System ~A not found." self))
        ((functionp default) (funcall default))
        (t default)))))

(defgeneric remove-system (self &key &allow-other-keys)
  (:method ((self system) &rest args)
    (apply 'remove-system (name self) args))
  (:method ((self t) &key)
    (with-system-session ()
      ;; freeze the session by acquiring the queue lock
      (with-queue-lock (system-session-systems *system-session*)
        (remhash self *system-table*)))))

(defgeneric load-system (self &key &allow-other-keys)
  (:documentation "Load the system SELF by ensuring all dependencies and components are loaded.")
  (:method ((self system) &key force (verbose t) (asdf *asdf-compatibility*) (init t))
    (when init (init self))
    (when verbose (mumble "Loading system ~A~@[ from ~A~]" (name self) (path self)))
    ;; TODO 2025-08-31:
    ;; - build-plan
    (if asdf 
        (asdf:load-system (name self) :verbose verbose :force force)
        (with-system-session (self)
          (case (plan self)
            ((or :serial nil) (mapc 'load-component (components self)))
            (t (nyi! "Unrecognized PLAN keyword"))))))
  (:method (self &rest args &key (default :error) (asdf *asdf-compatibility*))
    (remf args :default)
    (let ((sys (find-system self :default default :asdf asdf)))
      (apply 'load-system sys args)))
  (:method ((self asdf:system) &rest args)
    (apply 'asdf:load-system self args)))

(defgeneric compile-system (self &key &allow-other-keys)
  (:documentation "Compile system SELF.")
  (:method ((self system) &key (asdf *asdf-compatibility*) (verbose t) (init t))
    (when init (init self))
    (mumble "Compiling system ~A" (name self))
    (if asdf
        (asdf:compile-system (name self) :verbose verbose)
        (with-system-session (self)
          (mapc 'compile-component (components self)))))
  (:method ((self symbol) &rest args &key (default :error))
    (remf args :default)
    (apply 'compile-system (find-system self :default default) args)))

(defgeneric save-system (self &key &allow-other-keys)
  (:documentation "Save the system SELF."))

(defgeneric make-system (self &key &allow-other-keys)
  (:documentation "Make the system SELF which usually entails loading, compiling, and then saving
an image.")
  (:method ((self system) &key (asdf *asdf-compatibility*))
    (mumble "Making system ~A" (name self))
    (if asdf (asdf:make self :verbose nil)
        ;; else
        ))
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
  (:documentation "Test the system SELF.")
  (:method ((self system) &rest args &key (asdf *asdf-compatibility*))
    (remf args :asdf)
    (mumble "Testing system ~A" (name self))
    (if asdf
        (asdf:test-system self args)
        (progn (load-module (name self) :tests)
               (apply 'pkg:symbol-call *test-system* :do-suite (name self) args))))
  (:method ((self symbol) &rest args)
    (let ((sys (find-system self :default :error)))
      (apply #'test-system sys args))))
