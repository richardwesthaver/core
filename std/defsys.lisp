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

(defvar *system-table* (make-hash-table)
  "An EQL hash-table containing NAME:SYSTEM pairs.")

(defvar *provider-table* (make-hash-table)
  "A hash-table containing PROVIDER functions.")

(defvar *defining-system* nil
  "When non-nil, indicates the name of the system currently being defined.")

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

;;; Components
(defclass component () 
  ((name :initarg :name :accessor name)
   (path :initarg :path :accessor path)))

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

(defcomponent grovel-component (file-component) 
  (package)
  (:documentation "A FILE-COMPONENT which matches a SB-GROVEL constants file.")
  (:keyword :grovel))

;;; Compile Failures
(define-condition compile-condition (condition)
  ((context-format
    :initform nil :reader compile-condition-context-format :initarg :context-format)
   (context-arguments
    :initform nil :reader compile-condition-context-arguments :initarg :context-arguments)
   (description
    :initform nil :reader compile-condition-description :initarg :description))
  (:report (lambda (c s)
             (format s "~@<~A~@[ while ~?~]~@:>"
                     (or (compile-condition-description c) (type-of c))
                     (compile-condition-context-format c)
                     (compile-condition-context-arguments c)))))
(define-condition compile-file-error (compile-condition error) ())
(define-condition compile-warned-warning (compile-condition warning) ())
(define-condition compile-warned-error (compile-condition error) ())
(define-condition compile-failed-warning (compile-condition warning) ())
(define-condition compile-failed-error (compile-condition error) ())

(declaim ((member :warn :error :ignore) *compile-file-failure-action* *compile-file-warning-action*))
(defvar *compile-file-failure-action* :error)
(defvar *compile-file-warning-action* :warn)

(defun check-lisp-compile-warnings (warnings-p failure-p
                                    &optional context-format context-arguments)
  "Given the warnings or failures as resulted from COMPILE-FILE or checking deferred warnings,
raise an error or warning as appropriate"
  (when failure-p
    (case *compile-file-failure-action*
      (:warn (warn 'compile-failed-warning
                   :description "Lisp compilation failed"
                   :context-format context-format
                   :context-arguments context-arguments))
      (:error (error 'compile-failed-error
                     :description "Lisp compilation failed"
                     :context-format context-format
                     :context-arguments context-arguments))
      (:ignore nil)))
  (when warnings-p
    (case *compile-file-warning-action*
      (:warn (warn 'compile-warned-warning
                   :description "Lisp compilation had style-warnings"
                   :context-format context-format
                   :context-arguments context-arguments))
      (:error (error 'compile-warned-error
                     :description "Lisp compilation had style-warnings"
                     :context-format context-format
                     :context-arguments context-arguments))
      (:ignore nil))))

(defun check-lisp-compile-results (output warnings-p failure-p
                                   &optional context-format context-arguments)
  "Given the results of COMPILE-FILE, raise an error or warning as appropriate"
  (unless output
    (error 'compile-file-error :context-format context-format :context-arguments context-arguments))
  (check-lisp-compile-warnings warnings-p failure-p context-format context-arguments))

;;; Safe IO Syntax
;; TODO 2025-10-22: refactor?
(defvar *standard-readtable* (with-standard-io-syntax *readtable*)
  "The standard readtable, implementing the syntax specified by the CLHS.
It must never be modified, though only good implementations will even enforce that.")

(defmacro with-safe-io-syntax ((&optional (package :std)) &body body)
  "Establish safe CL reader options around the evaluation of BODY"
  `(call-with-safe-io-syntax #'(lambda () (let ((*package* (find-package ,package))) ,@body))))

(defun call-with-safe-io-syntax (thunk &key (package :std))
  (with-standard-io-syntax
    (let ((*package* (find-package package))
          (*read-default-float-format* 'double-float)
          (*print-readably* nil)
          (*read-eval* nil))
      (funcall thunk))))

(defun safe-read-from-string (string &key (package :cl) (eof-error-p t) eof-value (start 0) end preserve-whitespace)
  "Read from STRING using a safe syntax, as per WITH-SAFE-IO-SYNTAX"
  (with-safe-io-syntax (package)
    (read-from-string string eof-error-p eof-value :start start :end end :preserve-whitespace preserve-whitespace)))

;;; Deferred Warnings
(defun reify-undefined-warning (warning)
  ;; Extracting undefined-warnings from the compilation-unit
  ;; To be passed through the above reify/unreify link, it must be a "simple-sexp"
  (list*
   (sb-c::undefined-warning-kind warning)
   (sb-c::undefined-warning-name warning)
   (sb-c::undefined-warning-count warning)
   (mapcar
    #'(lambda (frob)
        ;; the lexenv slot can be ignored for reporting purposes
        `(:enclosing-source ,(sb-c::compiler-error-context-enclosing-source frob)
          :source ,(sb-c::compiler-error-context-source frob)
          :original-source ,(sb-c::compiler-error-context-original-source frob)
          :context ,(sb-c::compiler-error-context-context frob)
          :file-name ,(sb-c::compiler-error-context-file-name frob) ; a pathname
          :file-position ,(sb-c::compiler-error-context-file-position frob) ; an integer
          :original-source-path ,(sb-c::compiler-error-context-original-source-path frob)))
    (sb-c::undefined-warning-warnings warning))))

(defun reify-deferred-warnings ()
  "return a portable S-expression, portably readable and writeable in any Common Lisp implementation
using READ within a WITH-SAFE-IO-SYNTAX, that represents the warnings currently deferred by
WITH-COMPILATION-UNIT. One of three functions required for deferred-warnings support in ASDF."
  (when sb-c::*in-compilation-unit*
    ;; Try to send nothing through the pipe if nothing needs to be accumulated
    `(,@(when sb-c::*undefined-warnings*
          `((sb-c::*undefined-warnings*
             ,@(mapcar #'reify-undefined-warning sb-c::*undefined-warnings*))))
      ,@(loop :for what :in '(sb-c::*aborted-compilation-unit-count*
                              sb-c::*compiler-error-count*
                              sb-c::*compiler-warning-count*
                              sb-c::*compiler-style-warning-count*
                              sb-c::*compiler-note-count*)
              :for value = (symbol-value what)
              :when (plusp value)
              :collect `(,what . ,value)))))

(defun unreify-deferred-warnings (reified-deferred-warnings)
  "given a S-expression created by REIFY-DEFERRED-WARNINGS, reinstantiate the corresponding
deferred warnings as to be handled at the end of the current WITH-COMPILATION-UNIT.
Handle any warning that has been resolved already,
such as an undefined function that has been defined since.
One of three functions required for deferred-warnings support in ASDF."
  (declare (ignorable reified-deferred-warnings))
  (dolist (item reified-deferred-warnings)
    ;; Each item is (symbol . adjustment) where the adjustment depends on the symbol.
    ;; For *undefined-warnings*, the adjustment is a list of initargs.
    ;; For everything else, it's an integer.
    (destructuring-bind (symbol . adjustment) item
      (case symbol
        ((sb-c::*undefined-warnings*)
         (setf sb-c::*undefined-warnings*
               (nconc (mapcan
                       #'(lambda (stuff)
                           (destructuring-bind (kind name count . rest) stuff
                             (unless (case kind (:function (fboundp name)))
                               (list
                                (sb-c::make-undefined-warning
                                 :name name
                                 :kind kind
                                 :count count
                                 :warnings
                                 (mapcar #'(lambda (x)
                                             (apply #'sb-c::make-compiler-error-context x))
                                         rest))))))
                       adjustment)
                      sb-c::*undefined-warnings*)))
        (otherwise
         (set symbol (+ (symbol-value symbol) adjustment)))))))

(defun reset-deferred-warnings ()
  "Reset the set of deferred warnings to be handled at the end of the current
WITH-COMPILATION-UNIT."
  (when sb-c::*in-compilation-unit*
    (setf sb-c::*undefined-warnings* nil
          sb-c::*aborted-compilation-unit-count* 0
          sb-c::*compiler-error-count* 0
          sb-c::*compiler-warning-count* 0
          sb-c::*compiler-style-warning-count* 0
          sb-c::*compiler-note-count* 0)))

(defun save-deferred-warnings (warnings-file)
  "Save forward reference conditions so they may be issued at a latter time,
possibly in a different process."
  (with-open-file (s warnings-file :direction :output :if-exists :supersede
                                   :element-type 'character
                                   :external-format :utf-8)
    (with-safe-io-syntax (:cl)
      (let ((*read-eval* t))
        (write (reify-deferred-warnings) :stream s :pretty t :readably t)))
    (terpri s)))

(defun check-deferred-warnings (files &optional context-format context-arguments)
  "Given a list of FILES containing deferred warnings saved by CALL-WITH-SAVED-DEFERRED-WARNINGS,
re-intern and raise any warnings that are still meaningful."
  (let ((file-errors nil)
        (failure-p nil)
        (warnings-p nil))
    (handler-bind
        ((warning #'(lambda (c)
                      (setf warnings-p t)
                      (unless (typep c 'style-warning)
                        (setf failure-p t)))))
      (with-compilation-unit (:override t)
        (reset-deferred-warnings)
        (dolist (file files)
          (unreify-deferred-warnings
           (handler-case
               (with-safe-io-syntax ()
                 (let ((*read-eval* t))
                   (read-lisp-file file)))
             (error (c)
               ;;(delete-file-if-exists file) ;; deleting forces rebuild but prevents debugging
               (push c file-errors)
               nil))))))
    (dolist (error file-errors) (error error))
    (check-lisp-compile-warnings
     (or failure-p warnings-p) failure-p context-format context-arguments)))

(defun call-with-saved-deferred-warnings (thunk warnings-file &key source-namestring)
  "If WARNINGS-FILE is not nil, record the deferred-warnings around a call to THUNK
and save those warnings to the given file for latter use,
possibly in a different process. Otherwise just call THUNK."
  (declare (ignorable source-namestring))
  (if warnings-file
      (with-compilation-unit (:override t #+sbcl :source-namestring #+sbcl source-namestring)
        (unwind-protect
             (let (#+sbcl (sb-c::*undefined-warnings* nil))
               (multiple-value-prog1
                   (funcall thunk)
                 (save-deferred-warnings warnings-file)))
          (reset-deferred-warnings)))
      (funcall thunk)))

(defmacro with-saved-deferred-warnings ((warnings-file &key source-namestring) &body body)
  "Trivial syntax for CALL-WITH-SAVED-DEFERRED-WARNINGS"
  `(call-with-saved-deferred-warnings
    #'(lambda () ,@body) ,warnings-file :source-namestring ,source-namestring))

;;; Component Ops
;; Functions which are performed directly on instances of the COMPONENT class
;; in the calling thread.
(defun read-component (comp &key (external-format :default))
  "Read a component from its PATH slot."
  (std:read-lisp-file (path comp) :external-format external-format))

(defun compile-component (comp &rest args)
  "Compile a component."
  (apply 'compile-file (path comp) args))

(defun load-component (comp &rest args)
  "Load a component."
  (apply 'load (path comp) args))

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

(defprovider :io (name)
  name)

(defprovider :proto (name)
  name)

(defprovider :pool (name)
  `(find-thread-pool ,name))

(defprovider :pun (name)
  name)

;; TODO 2025-09-28: 
(defprovider :module (name &rest args)
  `(defmodule ,name ,@args))

;;; Module
;; Unlike MOD-COMPONENT/DIR-COMPONENT, based on ASDF:MODULE which is merely a
;; container for other COMPONENTs, Lisp Modules in the Core support the ANSI
;; CL notion of Modules and are further extended.

;; Modules in the core are essentially a 1:N mapping from an arbitrary name
;; (string or symbol) to tagged lisp objects we call providers. Providers are
;; designated by a keyword (the tag) and are responsible for calling a
;; function which provisions the associated lisp object.

;; The REQUIRE slot is a list of provider forms which indicate the
;; dependencies of the module.
(defvar *load-module* nil "The name of the module being loaded or NIL.")
(defvar *compile-module* nil "The name of the module being compiled or NIL.")
(defvar *module-stack* nil "A list of the most recently visited modules.")
(defvar *module* nil "The name of the current module or NIL.")
(defparameter *module-table* (make-hash-table :test 'equal)
  "A table which maps modules names to objects.")

(defclass module ()
  ((name :initarg :name :accessor name)
   (hook :initarg :hook :type hook :accessor hook)
   provide
   require)
  (:documentation "All Lisp Modules contain at least a NAME, HOOK, PROVIDE and REQUIRE slot."))

(defun load-module (name)
  (when-let ((*load-module* (gethash name *module-table*)))
    (with-slots (hook) *load-module*
      (when hook
        (pushnew (funcall hook :exit) sb-ext:*exit-hooks*)
        (funcall (funcall hook :load))))
    *load-module*))

;; TODO 2025-09-20: 
(defun partial-load-module (name &rest args)
  (declare (ignore args))
  (load-module name))

(defun unload-module () (setf *module* nil))

(defun module-provide-system (name)
  "Provide a SYSTEM, adding valid entries to the *MODULES* variable. The function
USE should be called in order to load and activate a module."
  (when-let ((sys (find-system name)))
    (load-system sys)
    t))

(pushnew 'module-provide-system sb-ext:*module-provider-functions*)

(defmacro with-module (name &body body)
  "Load the module named NAME, binding it to *MODULE* and eval BODY."
  `(let ((*module* (or (require ,name) ,name)))
     ,@body))

;; TODO 2025-09-28: 
(defmacro use (name &body body)
  "Load and activate module NAME with the provider forms in BODY."
  `(with-module ,name ,@body))

;; HACK 2025-09-28: 
;; refuse?

;; (with-eval-after-load (module &body body))

;;; System
(defcomponent system (mod-component module)
  ((version :accessor version)
   description
   (plan :description "The default plan associated with this object which specifies the ordering of
system jobs to be executed in an async context."
         :initform :serial))
  (:keyword :sys)
  (:default-initargs :hook (make-instance 'key-hook)))

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
     :components (mapcar #'change-component-class (asdf:component-children instance))))
  (((instance mod-component) (new-class-name (eql 'asdf:module)) &key)
   (make-instance new-class-name
     :name (name instance)
     :path (asdf:component-pathname instance)
     :components (mapcar #'revert-component-class (components instance))))
  ;; system
  (((instance asdf:system) (new-class-name (eql 'system)) &key)
   (make-instance new-class-name
     :version (asdf:component-version instance)
     :name (keywordicate (string-upcase (asdf:component-name instance)))
     :path (asdf:component-pathname instance)
     :description (asdf::component-description instance)
     :components (mapcar #'change-component-class (asdf:component-children instance))))
  (((instance system) (new-class-name (eql 'asdf:system)) &key)
   (warn 'simple-system-warning 
         :format-control "Erasing system slots (:require :provide :hook) from system ~A." 
         :format-arguments (name instance))
   (make-instance new-class-name
     :version (version instance)
     :name (name instance)
     :description (system-description instance)
     :components (mapcar #'revert-component-class (components instance)))))

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
    :components (mapcar #'change-component-class (asdf:component-children instance))))

(defcomponent bench-system (system) ()
  (:keyword :bench))

(defmethod change-class ((instance asdf:system) (new-class-name (eql 'bench-system)) &key)
  (make-instance new-class-name
    :version (asdf:component-version instance)
    :name (keywordicate (string-upcase (asdf:component-name instance)))
    :path (asdf:component-pathname instance)
    :description (asdf::component-description instance)
    :components (mapcar #'change-component-class (asdf:component-children instance))))

(defun bench-system-name-p (name)
  (std/seq:ends-with-subseq "/BENCH" (string-upcase name)))

(definline %bench-system-name (name)
  (concatenate 'simple-base-string (string-upcase name) "/BENCH"))

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

(defmacro with-system-session (&body body)
  "Bind *SYSTEM-SESSION* to a fresh value around BODY."
  `(progn
     (unless *system-session* (setf *system-session* (make-system-session)))
     ,@body))

;;; Defsys
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
     (if (atom x) ; default case, require the module
         (load-module x)
         (apply 'partial-load-module x))
     x)
   form))

(defun %parse-component-form (form)
  (if (atom form)
      (if (directory-path-p form)
          (make-instance 'dir-component 
            :include ".*" 
            :name (last (pathname-directory form))
            :path form)
          (make-instance 'file-component 
            :type (or (pathname-type form) "lisp") 
            :name (pathname-name form)
            :path form))
      (let ((n (cadr form))
            (kind (gethash (car form) *component-class-table*))
            (props (cddr form)))
        (ecase (car form)
          ((or :file :pkg :grovel)
           (let ((ty (or (pathname-type n) "lisp")))
             (make-instance kind
               :type (keywordicate (string-upcase ty))
               :name n
               :path (make-pathname :name n :type ty))))
          (:mod
           (let* ((path (directory-path n))
                  (*default-pathname-defaults* path))
             (make-instance kind
               :name n 
               :path path
               :components (mapcar '%parse-component-form (getf props :components)))))
          (:dir
           (let* ((path (directory-path n))
                  (*default-pathname-defaults* path)
                  (inc (or (getf props :include) (cl-ppcre:create-scanner ".*")))
                  (exc (getf props :exclude))
                  (c (make-instance kind
                       :name n
                       :path path
                       :include inc
                       :exclude exc
                       :components (mapcar '%parse-component-form (getf props :components)))))
             (walk-directory (path c)
               (constantly t) (constantly t)
               (lambda (x)
                 (dolist (f (directory-files x))
                   (let ((f (namestring f)))
                     (when (and (cl-ppcre:scan inc f) (or (not exc) (not (cl-ppcre:scan exc f))))
                       (push (%parse-component-form f) (components c)))))))
             c))))))

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
          (plan (or (%sys-get :plan) :serial))
          (class (or (%sys-get :class) ''system))
          (comp (%sys-get :components))
          (*defining-system* name))
      (declare (ignore meth))
      (std/sym:with-gensyms (sys)
        `(let ((,sys (change-class (defsystem ,name ,@body) ,class)))
           (setf (path ,sys) (or *compile-file-truename* *load-truename*)
                 (slot-value ,sys 'plan) ,plan
                 (slot-value ,sys 'components) ',(%parse-components-form comp)
                 (slot-value ,sys 'provide) ',(%parse-provide-form prov)
                 (slot-value ,sys 'require) ',(%parse-require-form req))
           (mapc (lambda (x) (add-hook (hook ,sys) x)) ',hooks)
           (register-system ,name ,sys))))))

(defun compile-sys (path)
  "Compile a SYS file at PATH. Default extension is FSYS."
  (compile-file path :output-file (make-pathname :name (pathname-name path) :type "fsys")
                     :entry-points '(load-sys)))

(defun load-sys (path &optional name)
  "Load a SYS file from PATH. Unlike LOAD-ASD this function calls LOAD
internally. On success the path is added to the *SYSDEFS* list."
  (let ((path (truename path)))
    (with-system-session
      (let ((*default-pathname-defaults* (pathname (directory-namestring path))))
        (when 
            (restart-case (load path)
              (load-file (p)
                :report "Load a different file." 
                :interactive (lambda () 
                               (list (setf path (interact-line "File: "))))
                (load p)))
          (setf (gethash path (system-session-file-cache *system-session*))
                (sb-ext:get-time-of-day))
          (pushnew (namestring (truename path)) *sysdefs* :test 'equal)
          (if name 
              (find-system name :default (lambda () (error 'defsys-load-error :name name :pathname path)))
              t))))))

;;; Protocol
(defmethod init ((self (eql :sys)) &key)
  (setq *system-table* (make-hash-table)
        *system-session* nil
        *sysdefs* nil
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
  (:method ((self system) &key force verbose)
    (when verbose (mumble "Loading system ~A~@[ from ~A~]" (name self) (path self)))
    ;; TODO 2025-08-31:
    (asdf:load-system (name self) :verbose verbose :force force))
  (:method ((self symbol) &rest args)
    (let ((sys (find-system self :default :error)))
      (apply 'load-system sys args))))

(defgeneric compile-system (self &key &allow-other-keys)
  (:documentation "Compile system SELF.")
  (:method ((self system) &key)
    (mumble "Compiling system ~A" (name self))
    (asdf:compile-system (name self) :verbose nil))
  (:method ((self symbol) &rest args)
    (apply 'compile-system (find-system self :default :error) args)))

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
  (:documentation "Test the system SELF.")
  (:method ((self system) &rest args)
    (mumble "Testing system ~A" (name self))
    (apply 'pkg:symbol-call :rt :do-suite (name self) args))
  (:method ((self symbol) &rest args)
    (let ((sys (find-system self :default :error)))
      (apply #'test-system sys args))))
